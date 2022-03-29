module Substitute
    ( substitute
    , ToRewriteForm (toRewriteForm)
    ) where

import Substitution (Substitution, SubstitutionElement)
import Substitution qualified
import MiniK
import Data.Kind (type Type)

-- Procedure for applying a substitution to a rewrite rule.
-- It checks that the given substitution contains all the
-- variables inside the rewrite rule, before actually
-- applying the substitution.
substitute :: Substitution -> RewriteVar -> Maybe RewriteRedex
substitute = flip substituteVariable

fromVar
    :: forall (term :: Variability -> Type)
    . ( SubstituteVariable term
      , ToRewriteForm term
      , SubstitutionElement (ValueForm term) )
    => Var term
    -> Substitution
    -> Maybe ((RewriteForm term) Concrete)
fromVar (KVar name) subst = toRewriteForm @term <$> Substitution.lookup name subst
fromVar (K term) subst = substituteVariable term subst

-- substitutions
type family ValueForm (term :: Variability -> Type)
    = (form :: Variability -> Type) | form -> term
  where
    ValueForm (OfMiniK Redex) = OfMiniK Value
    ValueForm OfIdType = OfIdType
    ValueForm OfIntType = OfIntType
    ValueForm OfBoolType = OfBoolType
    ValueForm (OfMapType Redex) = OfMapType Redex

-- AST for rewriting process
type family RewriteForm (term :: Variability -> Type)
    = (form :: Variability -> Type)
  where
    RewriteForm (OfRewrite Redex Value) = OfRewrite Redex Redex
    RewriteForm (OfKonfiguration Redex Value) = OfKonfiguration Redex Redex
    RewriteForm (OfMapType Redex) = OfMapType Redex
    RewriteForm (OfMapType Value) = OfMapType Redex
    RewriteForm (OfMiniK Redex) = OfMiniK Redex
    RewriteForm (OfKTerm Redex) = OfKTerm Redex
    RewriteForm Thunk = Thunk
    RewriteForm OfIdType = OfIdType
    RewriteForm OfIntType = OfIntType
    RewriteForm OfBoolType = OfBoolType

class SubstituteVariable (term :: Variability -> Type) where
    substituteVariable
        :: term Variable
        -> Substitution
        -> Maybe ((RewriteForm term) Concrete)

instance SubstituteVariable (OfRewrite Redex Value) where
    substituteVariable
        Rewrite { konf, condition }
        subst
      = Rewrite
        <$> substituteVariable konf subst
        <*> fromVar condition subst

instance SubstituteVariable (OfKonfiguration Redex Value) where
    substituteVariable
        Konfiguration { k, kState }
        subst
      = Konfiguration
        <$> substituteVariable k subst
        <*> fromVar (mapVar @Variable fromValueMap kState) subst

instance SubstituteVariable Thunk where
    substituteVariable (KVal kTerm) subst =
        KVal <$> fromVar kTerm subst
    substituteVariable (KSeq kTerm1 kTerm2) subst =
        KSeq
            <$> fromVar kTerm1 subst
            <*> fromVar kTerm2 subst

instance SubstituteVariable (OfMiniK Redex) where
    substituteVariable KEmpty _ = pure KEmpty
    substituteVariable (KSymbol symbolName terms rest) subst =
        KSymbol symbolName
            <$> flip substituteVariable subst `traverse` terms
            <*> substituteVariable rest subst

instance SubstituteVariable (OfKTerm Redex) where
    substituteVariable (KInt intTerm) subst =
        KInt <$> fromVar intTerm subst
    substituteVariable (KBool boolTerm) subst =
        KBool <$> fromVar boolTerm subst
    substituteVariable (KMap mapTerm) subst =
        KMap <$> fromVar mapTerm subst
    substituteVariable (KExp kTerm) subst =
        KExp <$> substituteVariable kTerm subst
        -- KExp . K <$> substituteVariable kTerm subst

instance SubstituteVariable OfIntType where
    substituteVariable (I val) _ = pure $ I val
    substituteVariable (Ref idTerm) subst =
        Ref <$> fromVar idTerm subst
    substituteVariable (Plus intTerm1 intTerm2) subst =
        Plus <$> fromVar intTerm1 subst
            <*> fromVar intTerm2 subst
    substituteVariable (Mod intTerm1 intTerm2) subst =
        Mod <$> fromVar intTerm1 subst
            <*> fromVar intTerm2 subst

-- instance SubstituteVariable OfIntValue where
--     substituteVariable (IVal val) _ = pure $ IVal val

instance SubstituteVariable OfIdType where
    substituteVariable (Id name) _ = pure $ Id name

instance SubstituteVariable OfBoolType where
    substituteVariable (B val) _ = pure $ B val
    substituteVariable (Not boolTerm) subst =
        Not <$> fromVar boolTerm subst
    substituteVariable (And boolTerm1 boolTerm2) subst =
        And <$> fromVar boolTerm1 subst
            <*> fromVar boolTerm2 subst
    substituteVariable (LT' intTerm1 intTerm2) subst =
        LT' <$> fromVar intTerm1 subst
            <*> fromVar intTerm2 subst

-- instance SubstituteVariable OfBoolValue where
--     substituteVariable (BVal val) _ = pure $ BVal val

instance SubstituteVariable (OfMapType Redex) where
    substituteVariable MapEmpty _ = pure $ MapEmpty
    substituteVariable (MapCons idTerm intTerm mapTerm) subst =
        MapCons <$> fromVar idTerm subst
            <*> fromVar intTerm subst
            <*> fromVar mapTerm subst

instance SubstituteVariable (OfMapType Value) where
    substituteVariable term = substituteVariable (fromValueMap term)

--

-- toRewriteFormVar
--     :: forall (v :: Variability) (term :: Evaluated -> Variability -> Type)
--     . (ToRewriteForm term, Variabilities v)
--     => Variab (term Value v)
--     -> Variab (term Redex v)
-- toRewriteFormVar = mapVar toRewriteForm

-- class ToRewriteForm (term :: Evaluated -> Variability -> Type) where
--     toRewriteForm
--         :: forall (v :: Variability)
--         . Variabilities v
--         => term Value v
--         -> term Redex v

class ToRewriteForm (term :: Variability -> Type) where
    toRewriteForm
        :: (ValueForm term) Concrete
        -> (RewriteForm term) Concrete

instance ToRewriteForm (OfMiniK Redex) where toRewriteForm = deNormalizeK
instance ToRewriteForm OfIdType where toRewriteForm = id
instance ToRewriteForm OfIntType where toRewriteForm = id
instance ToRewriteForm OfBoolType where toRewriteForm = id
instance ToRewriteForm (OfMapType Redex) where toRewriteForm = id

-- instance ToRewriteForm OfMiniK where
--     toRewriteForm KEmpty = KEmpty
--     toRewriteForm (KSymbol name body tail) = KSymbol name (toRewriteForm <$> body)
--         . KVal $ toRewriteFormVar tail

-- instance ToRewriteForm OfKTerm where
--     toRewriteForm (KInt term) = KInt $ term
--     toRewriteForm (KBool term) = KBool term
--     toRewriteForm (KMap term) = KMap term
--     toRewriteForm (KExp term) = KExp . KVal $ toRewriteFormVar term
