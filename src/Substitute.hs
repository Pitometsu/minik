module Substitute
    ( substitute
    ) where

import Substitution (Substitution)
import qualified Substitution
import MiniK
import qualified Control.Monad as Monad
import Data.Kind (type Type)

-- Procedure for applying a substitution to a rewrite rule.
-- It checks that the given substitution contains all the
-- variables inside the rewrite rule, before actually
-- applying the substitution.
substitute :: Substitution -> RewriteVar -> Maybe Rewrite
substitute = flip substituteVariable

fromVar :: Var term -> Substitution -> Maybe (term Concrete)
fromVar (Var name) subst = Substitution.lookup name subst
fromVar (K term) = substituteVariable term

class SubstituteVariable (term :: Variability -> Type) where
    substituteVariable
        :: term Variable
        -> Substitution
        -> Maybe (term Concrete)

instance SubstituteVariable OfRewrite where
    substituteVariable
        Rewrite { konf, condition }
        subst
      = Rewrite
        <$> substituteVariable konf subst
        <*> fromVar condition subst

instance SubstituteVariable (OfKonfiguration e) where
    substituteVariable
        Konfiguration { k, kState }
        subst
      = Konfiguration
        <$> fromVar k subst
        <*> fromVar kState subst

instance SubstituteVariable (Thunk e) where
    substituteVariable (KVal kTerm) subst =
        KVal <$> substituteVariable kTerm subst
    substituteVariable (KSeq kTerm1 kTerm2) subst =
        KSeq
            <$> fromVar kTerm1 subst
            <*> fromVar kTerm2 subst

instance SubstituteVariable (OfMiniK e) where
    substituteVariable KEmpty _ = pure KEmpty
    substituteVariable (KSymbol symbolName args rest) subst =
        KSymbol symbolName
            <$> flip substituteVariable subst `traverse` args
            <*> fromVar rest subst

instance SubstituteVariable (OfKTerm e) where
    substituteVariable (KInt intTerm) subst =
        KInt <$> fromVar intTerm subst
    substituteVariable (KBool boolTerm) subst =
        KBool <$> fromVar boolTerm subst
    substituteVariable (KMap mapTerm) subst =
        KMap <$> fromVar mapTerm subst
    substituteVariable (KExp kTerm) subst =
        KExp <$> fromVar kTerm subst

instance SubstituteVariable IntType where
    substituteVariable (I val) _ = pure $ I val
    substituteVariable (Ref idTerm) subst =
        Ref <$> fromVar idTerm subst
    substituteVariable (Plus intTerm1 intTerm2) subst =
        Plus <$> fromVar intTerm1 subst
            <*> fromVar intTerm2 subst
    substituteVariable (Mod intTerm1 intTerm2) subst =
        Mod <$> fromVar intTerm1 subst
            <*> fromVar intTerm2 subst

instance SubstituteVariable OfIntValue where
    substituteVariable (IVal val) _ = pure $ IVal val

instance SubstituteVariable IdType where
    substituteVariable (Id name) _ = pure $ Id name

instance SubstituteVariable BoolType where
    substituteVariable (B val) _ = pure $ B val
    substituteVariable (Not boolTerm) subst =
        Not <$> fromVar boolTerm subst
    substituteVariable (And boolTerm1 boolTerm2) subst =
        And <$> fromVar boolTerm1 subst
            <*> fromVar boolTerm2 subst
    substituteVariable (LT' intTerm1 intTerm2) subst =
        LT' <$> fromVar intTerm1 subst
            <*> fromVar intTerm2 subst

instance SubstituteVariable OfBoolValue where
    substituteVariable (BVal val) _ = pure $ BVal val

instance SubstituteVariable MapType where
    substituteVariable MapEmpty _ = pure $ MapEmpty
    substituteVariable (MapCons idTerm intTerm mapTerm) subst =
        MapCons <$> fromVar idTerm subst
            <*> fromVar intTerm subst
            <*> fromVar mapTerm subst
