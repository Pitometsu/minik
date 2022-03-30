module MiniK
    ( Name
    , OfIntValue (..)
    , IntValue
    , OfBoolValue (..)
    , BoolValue
    , Variability (..)
    , Variab
    , Var (..)
    , Evaluated (..)
    , NormalOf
    , Normal
    , Thunk (..)
    , OfMiniK (..)
    , MiniK
    , OfKTerm (..)
    , KTerm
    , normalizeK
    , deNormalizeK
    , fromValueMap
    , fromValueKonf
    , OfIdType (..)
    , IdType
    , OfIntType (..)
    , IntType
    , IntTypeVar
    , OfBoolType (..)
    , BoolType
    , OfMapType (..)
    , MapTypeRedex
    , OfKonfiguration (..)
    , Konfiguration
    , KonfigurationVarValue
    , KonfigurationConcr
    , KonfigurationRedex
    , canBeRewritten
    , RewriteRule (..)
    , rewriteOf
    , Variabilities (..)
    , OfRewrite (..)
    , RewriteVar
    , RewriteRedex
    , With
    , WithAll
    ) where

import Control.DeepSeq (type NFData)
import Data.Kind (type Constraint, type Type)
import Data.Ix (type Ix)
import GHC.Generics (type Generic)

type Name = String

newtype OfIntValue (v :: Variability) = IVal { intValue :: Int }
    deriving stock (Show, Eq, Ord, Bounded, Ix, Generic)
    deriving newtype (Enum, Num, Real, Integral)
    deriving anyclass NFData

type IntValue = OfIntValue Concrete

newtype OfBoolValue (v :: Variability) = BVal { boolValue :: Bool }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

type BoolValue = OfBoolValue Concrete

-- | Is certain term contain meta-variables
data Variability = Variable | Concrete
     deriving stock (Show, Eq, Ord, Generic)
     deriving anyclass NFData

-- to exclude PolyKind type's instances of a KnownVariability typeclass
type family ClosedKindVariability (v :: Variability)
    = (r :: Variability) | r -> v
  where
    ClosedKindVariability Variable = Variable
    ClosedKindVariability Concrete = Concrete

-- singletones to simplify constraints
data SingletoneVariability (v :: Variability) where
    SingletoneVariabilityVariable :: SingletoneVariability Variable
    SingletoneVariabilityConcrete :: SingletoneVariability Concrete

class v ~ ClosedKindVariability v => KnownVariability (v :: Variability) where
    knownVariability :: SingletoneVariability v

instance KnownVariability Variable where
    knownVariability = SingletoneVariabilityVariable

instance KnownVariability Concrete where
    knownVariability = SingletoneVariabilityConcrete

--

-- A bit of magic to be able to have a Variabilities typeclass with lazy
-- constraints for the VariabOf type.
data NotVar (v :: Variability) (t :: Variability -> Type)

type family UnNotVar (t :: Type) = (r :: Type) | r -> t where
    UnNotVar (NotVar v notVar) = notVar v
    UnNotVar (Var var) = Var var

type family ToVariab (v :: Variability) (t :: Variability -> Type)
    = (r :: (Variability -> Type) -> Type) | r -> v
  where
    ToVariab v Thunk = NotVar v
    ToVariab Concrete t = NotVar Concrete
    ToVariab Variable t = Var

type VariabOf (v :: Variability) (t :: Variability -> Type)
    = UnNotVar ((ToVariab v t) t)

type family Variab (t :: Type) where
  Variab (t v) = VariabOf v t

-- | Meta-variable of term
data Var (t :: Variability -> Type)
    = KVar !Name
    | K !(t Variable)

deriving stock instance With Show [a Variable, a Concrete] => Show (Var a)
deriving stock instance With Eq [a Variable, a Concrete] => Eq (Var a)
deriving stock instance With Ord [a Variable, a Concrete] => Ord (Var a)
deriving stock instance With Generic [a Variable, a Concrete] => Generic (Var a)
deriving anyclass instance
    WithAll [Generic, NFData] [a Variable, a Concrete] => NFData (Var a)

-- | Is certain term evaluated in normal form
data Evaluated = Redex | Value
     deriving stock (Show, Eq, Ord, Generic)
     deriving anyclass NFData

-- ???: r :: Variability -> Type
type family NormalOf (e :: Evaluated) (t :: Type) = (r :: Type) | r -> t

type instance NormalOf _ (OfIdType v) = OfIdType v

type instance NormalOf Redex (OfIntType v) = OfIntType v
type instance NormalOf Value (OfIntType v) = OfIntValue v
type instance NormalOf Redex (OfBoolType v) = OfBoolType v
type instance NormalOf Value (OfBoolType v) = OfBoolValue v

type family Normal (t :: Type) = (r :: Type)

type instance Normal (OfMiniK Redex v) = Thunk v
type instance Normal (OfMiniK Value v) = OfMiniK Value v

data Thunk (v :: Variability)
    = KVal !(Variab (OfMiniK Redex v))
    | KSeq !(Variab (OfMiniK Redex v)) (Variab (OfMiniK Redex v))
-- TODO: rewrite as a Functor of (OfKTerm e v).

deriving stock instance Show (Variab (OfMiniK Redex v)) => Show (Thunk v)
deriving stock instance Eq (Variab (OfMiniK Redex v)) => Eq (Thunk v)
deriving stock instance Ord (Variab (OfMiniK Redex v)) => Ord (Thunk v)
deriving stock instance Generic (Variab (OfMiniK Redex v)) => Generic (Thunk v)
deriving anyclass instance
    WithAll [Generic, NFData] '[Variab (OfMiniK Redex v)] => NFData (Thunk v)

-- The AST of the MiniK language for describing computations of
-- user-defined programming languages.
-- An overview of the constructors:
--   - KSeq and KEmpty allow sequencing computations
--   - KSymbol allows defining program constructs with arguments,
--   identified by the name of the symbol
--   - KInt, KBool, KMap are the only types allowed in programs
--   - KVar are variables which represent programs themselves
data OfMiniK (e :: Evaluated) (v :: Variability)
    = KEmpty
    | KSymbol !Name ![OfKTerm e v] (Variab (Normal (OfMiniK e v)))
-- TODO: rewrite as a Functor of (OfKTerm e v).

deriving stock instance
    With Show
        [ OfKTerm e v
        , Variab (Normal (OfMiniK e v)) ]
    => Show (OfMiniK e v)
deriving stock instance
    With Eq
        [ OfKTerm e v
        , Variab (Normal (OfMiniK e v)) ]
    => Eq (OfMiniK e v)
deriving stock instance
    With Ord
        [ OfKTerm e v
        , Variab (Normal (OfMiniK e v)) ]
    => Ord (OfMiniK e v)
deriving stock instance
    With Generic
        [ OfKTerm e v
        , Variab (Normal (OfMiniK e v)) ]
    => Generic (OfMiniK e v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ OfKTerm e v
        , Variab (Normal (OfMiniK e v)) ]
    => NFData (OfMiniK e v)

type MiniK = Variab (Normal (OfMiniK Value Concrete))

data OfKTerm (e :: Evaluated) (v :: Variability)
    = KInt !(VariabOf v OfIntType)
    | KBool !(VariabOf v OfBoolType)
    | KMap (VariabOf v (OfMapType Redex))
    | KExp !(Variab (Normal (OfMiniK e v)))
-- TODO: rewrite as a Functor of (OfKTerm e v).

deriving stock instance
    With Show
        [ VariabOf v OfIntType
        , VariabOf v OfBoolType
        , VariabOf v (OfMapType Redex)
        , Variab (Normal (OfMiniK e v)) ]
    => Show (OfKTerm e v)
deriving stock instance
    With Eq
        [ VariabOf v OfIntType
        , VariabOf v OfBoolType
        , VariabOf v (OfMapType Redex)
        , Variab (Normal (OfMiniK e v)) ]
    => Eq (OfKTerm e v)
deriving stock instance
    With Ord
        [ VariabOf v OfIntType
        , VariabOf v OfBoolType
        , VariabOf v (OfMapType Redex)
        , Variab (Normal (OfMiniK e v)) ]
    => Ord (OfKTerm e v)
deriving stock instance
    With Generic
        [ VariabOf v OfIntType
        , VariabOf v OfBoolType
        , VariabOf v (OfMapType Redex)
        , Variab (Normal (OfMiniK e v)) ]
    => Generic (OfKTerm e v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ VariabOf v OfIntType
        , VariabOf v OfBoolType
        , VariabOf v (OfMapType Redex)
        , Variab (Normal (OfMiniK e v)) ]
    => NFData (OfKTerm e v)

type KTerm = OfKTerm Value Concrete

normalizeK
    :: Variab (Normal (OfMiniK Redex Concrete))
    -> Variab (Normal (OfMiniK Value Concrete))
normalizeK term' = normalizeK' term'  KEmpty
  where
    normalizeK' (KSeq term1 term2) normalized = normalizeK' (KVal term1)
        $ normalizeK' (KVal term2) normalized
    normalizeK' (KVal KEmpty) normalized = normalized
    normalizeK' (KVal (KSymbol name body rest)) normalized =
        KSymbol name (normalizeTerm <$> body) $ normalizeK' rest normalized
    normalizeTerm :: OfKTerm Redex Concrete -> OfKTerm Value Concrete
    normalizeTerm (KExp term) = KExp $ normalizeK term
    normalizeTerm (KInt term) = KInt term
    normalizeTerm (KBool term) = KBool term
    normalizeTerm (KMap term) = KMap term

deNormalizeK
    :: OfMiniK Value Concrete
    -> OfMiniK Redex Concrete
deNormalizeK KEmpty = KEmpty
deNormalizeK (KSymbol name body rest) =
    KSymbol name (deNormalizeTerm <$> body) . KVal $ deNormalizeK rest
  where
    deNormalizeTerm (KExp term) = KExp . KVal $ deNormalizeK term
    deNormalizeTerm (KInt term) = KInt term
    deNormalizeTerm (KBool term) = KBool term
    deNormalizeTerm (KMap term) = KMap term

fromValueMap
    :: forall (v :: Variability)
    . KnownVariability v
    => OfMapType Value v
    -> OfMapType Redex v
fromValueMap MapEmpty = MapEmpty
fromValueMap (MapCons ind val rest) = case knownVariability @v of
    SingletoneVariabilityVariable
        -> MapCons
            (mapVar @Variable (Id . retractConcreteId) ind)
            (mapVar @Variable (I . IVal . intValue) val)
            $ mapVar @Variable (fromValueMap @v) rest
    SingletoneVariabilityConcrete
        -> MapCons
            (mapVar @Concrete (Id . retractConcreteId) ind)
            (mapVar @Concrete (I . IVal . intValue) val)
            $ mapVar @Concrete (fromValueMap @v) rest

fromValueKonf
    :: forall (e :: Evaluated)
        (v :: Variability)
    . KnownVariability v
    => OfKonfiguration e Value v
    -> OfKonfiguration e Redex v
fromValueKonf konfig = case knownVariability @v of
    SingletoneVariabilityVariable
        -> konfig { kState = fromValueMap @v `mapVar` kState konfig }
    SingletoneVariabilityConcrete
        -> konfig { kState = fromValueMap @v `mapVar` kState konfig }

-- A type for identifiers inside languages defined in MiniK.
-- For simplicity, languages may only have 'IntType' identifiers.
newtype OfIdType (v :: Variability) = Id { retractConcreteId :: Name }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

type IdType = OfIdType Concrete

data OfIntType (v :: Variability)
    = I !IntValue
    | Ref !(VariabOf v OfIdType)
    | Plus !(VariabOf v OfIntType) !(VariabOf v OfIntType)
    | Mod !(VariabOf v OfIntType) !(VariabOf v OfIntType)

deriving stock instance
    With Show
        [ VariabOf v OfIdType
        , VariabOf v OfIntType ]
    => Show (OfIntType v)
deriving stock instance
    With Eq
        [ VariabOf v OfIdType
        , VariabOf v OfIntType ]
    => Eq (OfIntType v)
deriving stock instance
    With Ord
        [ VariabOf v OfIdType
        , VariabOf v OfIntType ]
    => Ord (OfIntType v)
deriving stock instance
    With Generic
        [ VariabOf v OfIdType
        , VariabOf v OfIntType ]
    => Generic (OfIntType v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ VariabOf v OfIdType
        , VariabOf v OfIntType ]
    => NFData (OfIntType v)

type IntType = VariabOf Concrete OfIntType
type IntTypeVar = VariabOf Variable OfIntType

data OfBoolType (v :: Variability)
    = B !Bool
    | Not !(VariabOf v OfBoolType)
    | And !(VariabOf v OfBoolType) !(VariabOf v OfBoolType)
    | LT' !(VariabOf v OfIntType) !(VariabOf v OfIntType)

deriving stock instance
    With Show
        [ VariabOf v OfBoolType
        , VariabOf v OfIntType ]
    => Show (OfBoolType v)
deriving stock instance
    With Eq
        [ VariabOf v OfBoolType
        , VariabOf v OfIntType ]
    => Eq (OfBoolType v)
deriving stock instance
    With Ord
        [ VariabOf v OfBoolType
        , VariabOf v OfIntType ]
    => Ord (OfBoolType v)
deriving stock instance
    With Generic
        [ VariabOf v OfBoolType
        , VariabOf v OfIntType ]
    => Generic (OfBoolType v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ VariabOf v OfBoolType
        , VariabOf v OfIntType ]
    => NFData (OfBoolType v)

type BoolType = VariabOf Concrete OfBoolType
type BoolTypeVar = VariabOf Variable OfBoolType

-- A type for MiniK maps, used for storing the values identifiers
-- point to during the execution of a language defined in MiniK.
-- For simplicity, these values are restricted to 'IntType'.
data OfMapType (e :: Evaluated) (v :: Variability)
    = MapEmpty
    | MapCons
        !(Variab (NormalOf e (OfIdType v)))
        !(Variab (NormalOf e (OfIntType v)))
        !(Variab (OfMapType e v))

deriving stock instance
    With Show
        [ Variab (NormalOf e (OfIdType v))
        , Variab (NormalOf e (OfIntType v))
        , Variab (OfMapType e v) ]
    => Show (OfMapType e v)
deriving stock instance
    With Eq
        [ Variab (NormalOf e (OfIdType v))
        , Variab (NormalOf e (OfIntType v))
        , Variab (OfMapType e v) ]
    => Eq (OfMapType e v)
deriving stock instance
    With Ord
        [ Variab (NormalOf e (OfIdType v))
        , Variab (NormalOf e (OfIntType v))
        , Variab (OfMapType e v) ]
    => Ord (OfMapType e v)
deriving stock instance
    With Generic
        [ Variab (NormalOf e (OfIdType v))
        , Variab (NormalOf e (OfIntType v))
        , Variab (OfMapType e v) ]
    => Generic (OfMapType e v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ Variab (NormalOf e (OfIdType v))
        , Variab (NormalOf e (OfIntType v))
        , Variab (OfMapType e v) ]
    => NFData (OfMapType e v)

type MapTypeRedex = Variab (OfMapType Redex Concrete)

-- The MiniK program configuration: the 'k' component represents
-- the computation described in the MiniK language, and the
-- 'kState' component is the state in which the values of identifiers
-- are stored and modified during the execution of the 'k' computation.
--
-- In the documentation, you will see comments with the unparsed
-- representations of rewrite rules and configurations,
-- with syntax heavily inspired by the K Framework.
--
-- An example of such a configuration
--   <k> symbol1(123, Var1:Int, Var2:Bool) ~> symbol2(a) ~> Rest:K </k>
--   <kState> X:Id |-> a M:Map </kState>
--
-- Explanations:
--   - symbol1 and symbol2 are KSymbols with those respective names and arguments
--   - 123 is an IntType, (I 123)
--   - Var1 and Var2 are variables of IntType, respectively BoolType
--   - ~> is the KSeq constructor, for sequencing computations
--   - a is an identifier, IdType
--   - Rest is a KVar variable, here meaning the rest of the computations
--   - |-> is key-value association for MapType
--   - X is an IdType variable
--   - M is a MapType variable, here meaning the rest of the map
--
data OfKonfiguration (e :: Evaluated) (e' :: Evaluated) (v :: Variability) =
    Konfiguration
        { k :: !(Variab (Normal (OfMiniK e v)))
        , kState :: !(Variab (OfMapType e' v))
        }

deriving stock instance
    With Show
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType e' v) ]
    => Show (OfKonfiguration e e' v)
deriving stock instance
    With Eq
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType e' v) ]
    => Eq (OfKonfiguration e e' v)
deriving stock instance
    With Ord
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType e' v) ]
    => Ord (OfKonfiguration e e' v)
deriving stock instance
    With Generic
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType e' v) ]
    => Generic (OfKonfiguration e e' v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType e' v) ]
    => NFData (OfKonfiguration e e' v)

type Konfiguration = OfKonfiguration Value Value Concrete
type KonfigurationVar = OfKonfiguration Redex Value Variable
type KonfigurationVarValue = OfKonfiguration Value Value Variable
type KonfigurationConcr = OfKonfiguration Value Redex Concrete
type KonfigurationRedex = OfKonfiguration Redex Redex Concrete

-- Can the program be rewritten further?
canBeRewritten
    :: forall (e' :: Evaluated) . OfKonfiguration Value e' Concrete -> Bool
canBeRewritten Konfiguration { k } =
    case k of
        KEmpty -> False
        _ -> True

-- Rewrite rules model program configuration transformations: the
-- 'left' rewrites to the 'right'. Therefore program execution in
-- MiniK is just a sequence of rewrite rule applications.
-- Rewrite rules are conditional, meaning that they only apply
-- if the 'sideCondition' is true.
--
-- In the documentation, you will see comments with the unparsed
-- representations of rewrite rules and configurations,
-- with syntax heavily inspired by the K Framework:
--    rule left => right requires sideCondition
--
data RewriteRule =
    RewriteRule
        { left :: !KonfigurationVarValue
        , right :: !KonfigurationVar
        , sideCondition :: !BoolTypeVar
        }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

rewriteOf :: RewriteRule -> OfRewrite Redex Value Variable
rewriteOf RewriteRule { right, sideCondition } =
    Rewrite (Konfiguration { k = k right, kState = kState right }) sideCondition

-- mapVar machinery

class toVar ~ ToVariab v a => Variabilities
    (v :: Variability)
    (a :: Variability -> Type)
    (toVar :: (Variability -> Type) -> Type)
  where
    mapVar
        :: forall (b :: Variability -> Type)
        . (a v -> b v)
        -> UnNotVar (toVar a)
        -> UnNotVar (toVar b)

instance
    (toVar ~ ToVariab v a
    , toVar ~ NotVar v)
    => Variabilities v a (NotVar v)
  where
    mapVar
        :: forall (b :: Variability -> Type)
        . (a v -> b v)
        -> UnNotVar (toVar a)
        -> UnNotVar (toVar b)
    mapVar = mapVarOf @v @a @b @(a v) @(b v)

instance
    (toVar ~ ToVariab v a
    , toVar ~ Var
    , v ~ Variable)
    => Variabilities Variable a Var
  where
    mapVar
        :: forall (b :: Variability -> Type)
        . (a v -> b v)
        -> UnNotVar (toVar a)
        -> UnNotVar  (toVar b)
    mapVar = mapVarOf @v @a @b @(Var a) @(Var b)

class VariabilitiesOf (v :: Variability) (a :: Variability -> Type) (b :: Variability -> Type) (ra :: Type) (rb :: Type) where
    mapVarOf
        :: (a v -> b v)
        -> ra
        -> rb

instance VariabilitiesOf Variable a b (Var a) (Var b) where
    mapVarOf f = \case
        KVar name -> KVar name
        K term -> K $ f term
instance VariabilitiesOf v a b (a v) (b v) where
    mapVarOf = ($)

--

data OfRewrite (e :: Evaluated) (e' :: Evaluated) (v :: Variability) = Rewrite
    { konf :: !(OfKonfiguration e e' v)
    , condition :: !(VariabOf v OfBoolType)
    }

deriving stock instance
    With Show
        [ OfKonfiguration e e' v
        , VariabOf v OfBoolType ]
    => Show (OfRewrite e e' v)
deriving stock instance
    With Eq
        [ OfKonfiguration e e' v
        , VariabOf v OfBoolType ]
    => Eq (OfRewrite e e' v)
deriving stock instance
    With Ord
        [ OfKonfiguration e e' v
        , VariabOf v OfBoolType ]
    => Ord (OfRewrite e e' v)
deriving stock instance
    With Generic
        [ OfKonfiguration e e' v
        , VariabOf v OfBoolType ]
    => Generic (OfRewrite e e' v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ OfKonfiguration e e' v
        , VariabOf v OfBoolType ]
    => NFData (OfRewrite e e' v)

type RewriteVar = OfRewrite Redex Value Variable
type RewriteRedex = OfRewrite Redex Redex Concrete

-- auxiliary machinery

type With :: forall typ. (typ -> Constraint) -> [typ] -> Constraint
type family With constraint types where
    With _ '[] = ()
    With constraint (typ : rest) = (constraint typ, With constraint rest)

type WithAll :: forall typ. [typ -> Constraint] -> [typ] -> Constraint
type family WithAll constraints types where
    WithAll '[] _ = ()
    WithAll (constraint : rest) types = (With constraint types, WithAll rest types)
