module Match
    ( match
    ) where

import MiniK
import Substitution (type Substitution, type SubstitutionElement)
import Control.Monad (guard)
import Control.Monad.Logic (LogicT)
import Control.Monad.Logic qualified as Logic
import Control.Monad.Trans.Class (lift)
import Control.Parallel.Strategies (parTraversable, rdeepseq, using)
import Substitution qualified
import Control.Applicative (Alternative (..))
import NormalizedMap qualified
import Data.Map.Lazy qualified as Map
import Data.Kind (type Type)
import NormalizedMap (OfNormalizedMap (..), OfMap (..), MapOf (..), NormalizedMapOf (..), NormalizedOf (..), OfNormalized (..), MapOfInt)
-- import Debug.Trace

-- Matching is the heart of the rewrite engine.
--
-- The left hand side of the rule is matched against the program.
-- Since we are doing concrete execution, the program will be fully concrete
-- (it will not contain variables).
--
-- This results in a substitution, where we associate the variables in the
-- rule's left hand side to terms present in the program configuration.
--
-- This implements mostly syntactical matching, with the exception of the
-- map case, where we need to account for map associativity and commutativity.
match :: KonfigurationConcr -> KonfigurationVarValue -> Maybe Substitution
match
    Konfiguration { k = kKonfig, kState = kStateKonfig }
    Konfiguration { k = kRule, kState = kStateRule }
  =
    matchVar kKonfig kRule Substitution.empty
    >>= matchVar kStateKonfig (mapVar @Variable fromValueMap kStateRule)

matchVar
    :: forall (term :: Variability -> Type)
    . (Match term, SubstitutionElement term)
    => term Concrete
    -> Var term
    -> Substitution
    -> Maybe Substitution
matchVar konfigTerm (K ruleTerm) subst
    = matchWithSubstitution konfigTerm ruleTerm subst
matchVar konfigTerm (KVar name) subst
    = pure $ Substitution.insert name konfigTerm subst

class Match (term :: Variability -> Type) where
    matchWithSubstitution
        :: term Concrete
        -> term Variable
        -> Substitution
        -> Maybe Substitution

instance Match (OfMiniK Value) where
    matchWithSubstitution KEmpty KEmpty subst =
        -- traceEvent "Match \"KEmpty\"" $
        pure subst
    matchWithSubstitution
        (KSymbol konfigSymbolName konfigArgs konfigTerm)
        (KSymbol ruleSymbolName ruleArgs ruleTerm)
        subst
        | konfigSymbolName == ruleSymbolName
        , length konfigArgs == length ruleArgs
      =
        -- traceEvent "Match \"KSymbol\"" $
        Substitution.multiUnion
        <$> ((:) <$> matchVar konfigTerm ruleTerm subst <*> traverse
            (\(kArg, rArg) -> matchWithSubstitution kArg rArg subst)
            (zip konfigArgs ruleArgs)) `using` parTraversable rdeepseq
        | otherwise = Nothing
    matchWithSubstitution _ _ _ = Nothing

instance Match (OfKTerm Value) where
    matchWithSubstitution (KInt intTermKonfig) (KInt intTermRule) subst =
        -- traceEvent "Match \"KInt\"" $
        matchVar intTermKonfig intTermRule subst
    matchWithSubstitution (KBool boolTermKonfig) (KBool boolTermRule) subst =
        -- traceEvent "Match \"KBool\"" $
        matchVar boolTermKonfig boolTermRule subst
    matchWithSubstitution (KMap mapTermKonfig) (KMap mapTermRule) subst =
        -- traceEvent "Match \"KMap\"" $
        matchVar mapTermKonfig mapTermRule subst
    matchWithSubstitution (KExp termKonfig) (KExp termRule) subst =
        -- traceEvent "Match \"KExp\"" $
        matchVar termKonfig termRule subst
    matchWithSubstitution _ _ _ = Nothing

instance Match OfIdType where
    matchWithSubstitution (Id name1) (Id name2) subst
        = guard (name1 == name2) >> pure subst

-- Note: 'Plus' matching is not yet associative or commutative.
instance Match OfIntType where
    matchWithSubstitution (I konfigVal) (I ruleVal) subst
        = guard (konfigVal == ruleVal) >> pure subst
    matchWithSubstitution (Ref konfigIdTerm) (Ref ruleIdTerm) subst =
        matchVar konfigIdTerm ruleIdTerm subst
    matchWithSubstitution
        (Plus konfigIdTerm1 konfigIdTerm2)
        (Plus ruleIdTerm1 ruleIdTerm2)
        subst
      = do
        subst1 <- matchVar konfigIdTerm1 ruleIdTerm1 subst
        subst2 <- matchVar konfigIdTerm2 ruleIdTerm2 subst
        pure $ Substitution.union subst1 subst2
    matchWithSubstitution
        (Mod konfigIdTerm1 konfigIdTerm2)
        (Mod ruleIdTerm1 ruleIdTerm2)
        subst
      = do
        subst1 <- matchVar konfigIdTerm1 ruleIdTerm1 subst
        subst2 <- matchVar konfigIdTerm2 ruleIdTerm2 subst
        pure $ Substitution.union subst1 subst2
    matchWithSubstitution _ _ _ = Nothing

instance Match OfBoolType where
    matchWithSubstitution (B val1) (B val2) subst
        = guard (val1 == val2) >> pure subst
    matchWithSubstitution (Not boolTermKonfig) (Not boolTermRule) subst =
        matchVar boolTermKonfig boolTermRule subst
    matchWithSubstitution
        (And boolTermKonfig1 boolTermKonfig2)
        (And boolTermRule1 boolTermRule2)
        subst
      = do
        subst1 <- matchVar boolTermKonfig1 boolTermRule1 subst
        subst2 <- matchVar boolTermKonfig2 boolTermRule2 subst
        pure $ Substitution.union subst1 subst2
    matchWithSubstitution
        (LT' intTermKonfig1 intTermKonfig2)
        (LT' intTermRule1 intTermRule2)
        subst
      = do
        subst1 <- matchVar intTermKonfig1 intTermRule1 subst
        subst2 <- matchVar intTermKonfig2 intTermRule2 subst
        pure $ Substitution.union subst1 subst2
    matchWithSubstitution _ _ _ = Nothing

instance Match (OfMapType Redex) where
    matchWithSubstitution MapEmpty MapEmpty subst = pure subst
    matchWithSubstitution
        konfigMap@MapCons {}
        ruleMap@MapCons {}
        subst
      = do
        let normalizedKonfigMap = NormalizedMapOf . NormalizedOf
                $ NormalizedMap.normalize konfigMap
            normalizedRuleMap = NormalizedMapOf . NormalizedOf
                $ NormalizedMap.normalize ruleMap
        matchWithSubstitution normalizedKonfigMap normalizedRuleMap subst
    matchWithSubstitution _ _ _ = Nothing

instance Match (NormalizedMapOf Redex) where
    matchWithSubstitution
        (NormalizedMapOf (NormalizedOf (OfMap concreteKonfig)))
        (NormalizedMapOf (NormalizedOf (NormalizedMap OfNormalized
            { opaque = opaqueRule
            , symbolic = symbolicRule
            , concrete = concreteRule
            })))
        subst
        =
        -- traceEvent "Match \"NormalizedMap\"" $
        do
            (leftToMatchSymbolic, matchedWithConcrete) <-
                matchWithConcrete concreteKonfig concreteRule
            (leftToMatchOpaque, matchedWithSymbolic) <-
                matchWithSymbolic leftToMatchSymbolic symbolicRule
            matchedWithOpaque <- matchWithOpaque leftToMatchOpaque opaqueRule
            return
                $ Substitution.multiUnion
                    [ matchedWithConcrete
                    , matchedWithSymbolic
                    , matchedWithOpaque
                    , subst
                    ]
      where -- TODO: encapsulate NormalizedMap logic into the module
        matchWithConcrete
            :: MapOfInt Concrete Redex
            -> MapOfInt Variable Redex
            -> Maybe (MapOfInt Concrete Redex, Substitution)
        matchWithConcrete (MapOf konfigMap) (MapOf ruleMap) =
          -- traceEvent "Match \"NormalizedMap\" with concrete ids" $
          do
            let
                -- ???: more elegant way to split values out
                intersectionKonfigValues = Map.elems
                    $ Map.intersection konfigMap ruleMap
                intersectionRuleValues = Map.elems
                    $ Map.intersection ruleMap konfigMap
                difference = Map.difference konfigMap ruleMap
            matchedValues <-
                traverse
                    (uncurry matchElements)
                    (zip intersectionKonfigValues intersectionRuleValues)
                `using` parTraversable rdeepseq
            return (MapOf difference, Substitution.multiUnion matchedValues)

        matchWithSymbolic
            :: MapOfInt Concrete Redex
            -> MapOfInt Variable Redex
            -> Maybe (MapOfInt Concrete Redex, Substitution)
        matchWithSymbolic (MapOf konfigMap) (MapOf ruleMap) =
          -- traceEvent "Match \"NormalizedMap\" with symbolic vars" $
          do
            matchResult <- Logic.observeT
                $ generateMatch (Map.toList konfigMap) (Map.toList ruleMap)
            let difference =
                    Map.withoutKeys konfigMap (Substitution.getIds matchResult)
            return (MapOf difference, matchResult)

        matchWithOpaque
            :: MapOfInt Concrete Redex
            -> Maybe Name
            -> Maybe Substitution
        matchWithOpaque _ Nothing = Nothing
        matchWithOpaque konfigMap (Just varName) =
          -- traceEvent "Match \"NormalizedMap\" with an opaque map" $
            let unNormalizedMap = NormalizedMap.unNormalize @Concrete $ OfMap konfigMap in
                Just $ Substitution.insert varName unNormalizedMap subst

        generateMatch
            :: [(Name, IntType)]
            -> [(Name, IntTypeVar)]
            -> LogicT Maybe Substitution
        generateMatch mapKonfig mapRule = do
            (idName, intTermKonfig) <- scatter mapKonfig
            (varName, intTermRule) <- scatter mapRule
            keySubst <- lift
                $ matchVar @OfIdType (Id idName) (KVar varName) subst
            valueSubst <- lift
                $ matchVar @OfIntType intTermKonfig intTermRule subst
            pure $ Substitution.union keySubst valueSubst

        matchElements elem1 elem2 =
            matchVar elem1 elem2 subst

scatter :: (Foldable f, Alternative m) => f a -> m a
scatter = foldr ((<|>) . pure) empty
