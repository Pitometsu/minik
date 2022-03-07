module Match
    ( match
    ) where

import MiniK
import Substitution (Substitution)
import Control.Monad.Logic (LogicT)
import qualified Control.Monad.Logic as Logic
import Control.Monad.Trans.Class (lift)
import Control.Parallel.Strategies (parTraversable, rdeepseq, using)
import qualified Substitution
import Control.Applicative (Alternative (..))
import qualified NormalizedMap
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Data.Function ((&))
import NormalizedMap (NormalizedMap (..))
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
match :: Konfiguration -> Konfiguration -> Maybe Substitution
match
    Konfiguration { k = kKonfig, kState = kStateKonfig }
    Konfiguration { k = kRule, kState = kStateRule }
  =
    matchWithSubstitution kKonfig kRule Substitution.empty
    >>= matchWithSubstitution kStateKonfig kStateRule

class Match term where
    matchWithSubstitution :: term -> term -> Substitution -> Maybe Substitution

instance Match MiniK where
    matchWithSubstitution kTerm (KVar name) subst =
        -- traceEvent "Match \"KVar\"" $
        Just (Substitution.insert name kTerm subst)
    matchWithSubstitution KEmpty KEmpty subst =
        -- traceEvent "Match \"KEmpty\"" $
        Just subst
    matchWithSubstitution (KInt intTermKonfig) (KInt intTermRule) subst =
        -- traceEvent "Match \"KInt\"" $
        matchWithSubstitution intTermKonfig intTermRule subst
    matchWithSubstitution (KBool boolTermKonfig) (KBool boolTermRule) subst =
        -- traceEvent "Match \"KBool\"" $
        matchWithSubstitution boolTermKonfig boolTermRule subst
    matchWithSubstitution (KMap mapTermKonfig) (KMap mapTermRule) subst =
        -- traceEvent "Match \"KMap\"" $
        matchWithSubstitution mapTermKonfig mapTermRule subst

    matchWithSubstitution
        (KSymbol konfigSymbolName konfigArgs)
        (KSymbol ruleSymbolName ruleArgs)
        subst
        | konfigSymbolName == ruleSymbolName
        , length konfigArgs == length ruleArgs
      =
        Substitution.multiUnion
        <$> traverse
            (\(kArg, rArg) -> matchWithSubstitution kArg rArg subst)
            (zip konfigArgs ruleArgs) `using` parTraversable rdeepseq
        | otherwise = Nothing

    matchWithSubstitution
        (KSeq konfigTerm1 konfigTerm2)
        (KSeq ruleTerm1 ruleTerm2)
        subst
      = do
        subst1 <- matchWithSubstitution konfigTerm1 ruleTerm1 subst
        subst2 <- matchWithSubstitution konfigTerm2 ruleTerm2 subst
        pure $ Substitution.union subst1 subst2

    matchWithSubstitution _ _ _ = Nothing

-- Note: 'Plus' matching is not yet associative or commutative.
instance Match IntType where
    matchWithSubstitution intTerm (IntVar name) subst =
        Just (Substitution.insert name (KInt intTerm) subst)
    matchWithSubstitution (I val1) (I val2) subst
        | val1 == val2 = Just subst
        | otherwise = Nothing
    matchWithSubstitution (IntId konfigIdTerm) (IntId ruleIdTerm) subst =
        matchWithSubstitution konfigIdTerm ruleIdTerm subst

    matchWithSubstitution
        (Plus konfigIdTerm1 konfigIdTerm2)
        (Plus ruleIdTerm1 ruleIdTerm2)
        subst
      = do
        subst1 <- matchWithSubstitution konfigIdTerm1 ruleIdTerm1 subst
        subst2 <- matchWithSubstitution konfigIdTerm2 ruleIdTerm2 subst
        pure $ Substitution.union subst1 subst2

    matchWithSubstitution
        (Mod konfigIdTerm1 konfigIdTerm2)
        (Mod ruleIdTerm1 ruleIdTerm2)
        subst
      = do
        subst1 <- matchWithSubstitution konfigIdTerm1 ruleIdTerm1 subst
        subst2 <- matchWithSubstitution konfigIdTerm2 ruleIdTerm2 subst
        pure $ Substitution.union subst1 subst2

    matchWithSubstitution _ _ _ = Nothing

instance Match IdType where
    matchWithSubstitution idTerm (IdVar name) subst =
        Just (Substitution.insert name (KInt (IntId idTerm)) subst)
    matchWithSubstitution (Id name1) (Id name2) subst
        | name1 == name2 = Just subst
        | otherwise = Nothing
    matchWithSubstitution _ _ _ = Nothing

instance Match BoolType where
    matchWithSubstitution boolTerm (BoolVar name) subst =
        Just (Substitution.insert name (KBool boolTerm) subst)

    matchWithSubstitution (B val1) (B val2) subst
        | val1 == val2 = Just subst
        | otherwise = Nothing

    matchWithSubstitution (Not boolTermKonfig) (Not boolTermRule) subst =
        matchWithSubstitution boolTermKonfig boolTermRule subst

    matchWithSubstitution
        (And boolTermKonfig1 boolTermKonfig2)
        (And boolTermRule1 boolTermRule2)
        subst
      = do
        subst1 <- matchWithSubstitution boolTermKonfig1 boolTermRule1 subst
        subst2 <- matchWithSubstitution boolTermKonfig2 boolTermRule2 subst
        pure $ Substitution.union subst1 subst2

    matchWithSubstitution
        (LT' intTermKonfig1 intTermKonfig2)
        (LT' intTermRule1 intTermRule2)
        subst
      = do
        subst1 <- matchWithSubstitution intTermKonfig1 intTermRule1 subst
        subst2 <- matchWithSubstitution intTermKonfig2 intTermRule2 subst
        pure $ Substitution.union subst1 subst2

    matchWithSubstitution _ _ _ = Nothing

instance Match MapType where
    matchWithSubstitution mapTerm (MapVar name) subst =
        Just (Substitution.insert name (KMap mapTerm) subst)
    matchWithSubstitution MapEmpty MapEmpty subst = Just subst

    matchWithSubstitution
        konfigMap@MapCons {}
        ruleMap@MapCons {}
        subst
      = do
        let normalizedKonfigMap = NormalizedMap.normalize konfigMap
            normalizedRuleMap = NormalizedMap.normalize ruleMap
        matchWithSubstitution normalizedKonfigMap normalizedRuleMap subst

    matchWithSubstitution _ _ _ = Nothing

instance Match NormalizedMap where
    matchWithSubstitution
        NormalizedMap
            { opaque = opaqueKonfig
            , symbolic = symbolicKonfig
            , concrete = concreteKonfig
            }
        NormalizedMap
            { opaque = opaqueRule
            , symbolic = symbolicRule
            , concrete = concreteRule
            }
        subst
        -- We expect the program configuration to be fully concrete.
        | not (null opaqueKonfig && Map.null symbolicKonfig) =
            -- traceEvent "Match \"NormalizedMap\", not concrete"
            Nothing
        | otherwise =
        -- traceEvent "Match \"NormalizedMap\"" $
        do
            (leftToMatch1, matchedWithConcrete) <-
                matchWithConcrete concreteKonfig concreteRule
            (leftToMatch2, matchedWithSymbolic) <-
                matchWithSymbolic leftToMatch1 symbolicRule
            matchedWithOpaque <- matchWithOpaque leftToMatch2 opaqueRule
            return
                $ Substitution.multiUnion
                    [ matchedWithConcrete
                    , matchedWithSymbolic
                    , matchedWithOpaque
                    , subst
                    ]
      where
        matchWithConcrete
            :: Map Name IntType
            -> Map Name IntType
            -> Maybe (Map Name IntType, Substitution)
        matchWithConcrete mapKonfig mapRule =
          -- traceEvent "Match \"NormalizedMap\" with concrete ids" $
          do
            let intersectionKonfigValues =
                    Map.intersection mapKonfig mapRule
                    & Map.elems
                intersectionRuleValues =
                    Map.intersection mapRule mapKonfig
                    & Map.elems
                difference = Map.difference mapKonfig mapRule
            matchedValues <-
                traverse
                    (uncurry matchElements)
                    (zip intersectionKonfigValues intersectionRuleValues)
                `using` parTraversable rdeepseq
            return (difference, Substitution.multiUnion matchedValues)

        matchWithSymbolic
            :: Map Name IntType
            -> Map Name IntType
            -> Maybe (Map Name IntType, Substitution)
        matchWithSymbolic mapKonfig mapRule =
          -- traceEvent "Match \"NormalizedMap\" with symbolic vars" $
          do
            matchResult <-
                generateMatch (Map.toList mapKonfig) (Map.toList mapRule)
                & Logic.observeT
            let difference =
                    Map.withoutKeys mapKonfig (Substitution.getIds matchResult)
            return (difference, matchResult)

        matchWithOpaque
            :: Map Name IntType
            -> Maybe Name
            -> Maybe Substitution
        matchWithOpaque _ Nothing = Nothing
        matchWithOpaque konfigMap (Just varName) =
          -- traceEvent "Match \"NormalizedMap\" with an opaque map" $
            let unNormalizedMap =
                    KMap
                    . NormalizedMap.unNormalize
                    . NormalizedMap.fromConcrete
                    $ konfigMap
             in Substitution.insert varName unNormalizedMap subst
                & Just

        generateMatch
            :: [(Name, IntType)]
            -> [(Name, IntType)]
            -> LogicT Maybe Substitution
        generateMatch mapKonfig mapRule = do
            (idName, intTermKonfig) <- scatter mapKonfig
            (varName, intTermRule) <- scatter mapRule
            keySubst <-
                matchWithSubstitution (IntId (Id idName)) (IntVar varName) subst
                & lift
            valueSubst <-
                matchWithSubstitution intTermKonfig intTermRule subst
                & lift
            pure $ Substitution.union keySubst valueSubst

        matchElements elem1 elem2 =
            matchWithSubstitution elem1 elem2 subst

scatter :: (Foldable f, Alternative m) => f a -> m a
scatter = foldr ((<|>) . pure) empty
