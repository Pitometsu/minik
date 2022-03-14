module MiniK
    -- ( MiniK (..)
    -- , IdType (..)
    -- , IntType (..)
    -- , BoolType (..)
    -- , MapType (..)
    -- , Konfiguration (..)
    -- , RewriteRule (..)
    -- , Name
    -- , retractIntTerm
    -- , retractBoolTerm
    -- , retractIdTerm
    -- , retractMapTerm
    -- , retractConcreteId
    -- , retractConcreteMap
    -- , extractConcreteId
    -- , canBeRewritten
    -- , normalizeK
    -- , deNormalizeK
    -- , reNormalizeK
    -- ) where
    where

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

-- deriving stock instance Generic IntValue

newtype OfBoolValue (v :: Variability) = BVal { boolValue :: Bool }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

type BoolValue = OfBoolValue Concrete

-- | Is certain term contain meta-variables
data Variability = Variable | Concrete
     deriving stock (Show, Eq, Ord, Generic)
     deriving anyclass NFData

type family VariabOf (v :: Variability) (t :: Variability -> Type) where
  VariabOf v Thunk = Thunk v
  VariabOf Concrete t = t Concrete
  VariabOf Variable t = Var t

type family Variab (t :: Type) where
  Variab (t v) = VariabOf v t

-- | Meta-variable of term
data Var (t :: Variability -> Type)
    = KVar !Name
    | K (t Variable)
    | KAll (t Concrete)

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

type family NormalOf (e :: Evaluated) (t :: Type)

type instance NormalOf _ (OfIdType v) = OfIdType v
type instance NormalOf Redex (OfIntType v) = OfIntType v
type instance NormalOf Value (OfIntType v) = OfIntValue v
type instance NormalOf Redex (OfBoolType v) = OfBoolType v
type instance NormalOf Value (OfBoolType v) = OfBoolValue v
-- type instance NormalOf _ (Var t) = Var t

type family Normal (t :: Type)

type instance Normal (OfMiniK Redex v) = Thunk v
type instance Normal (OfMiniK Value v) = OfMiniK Value v

data Thunk (v :: Variability)
    = KVal !(Variab (OfMiniK Redex v))
    | KSeq !(Variab (OfMiniK Redex v)) (Variab (OfMiniK Redex v))

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
    -- | name, body, tail
    | KSymbol !Name ![OfKTerm e v] (Variab (Normal (OfMiniK e v)))

deriving stock instance
    With Show
        [ Variab (Normal (OfMiniK e v))
        , OfKTerm e v ]
    => Show (OfMiniK e v)
deriving stock instance
    With Eq
        [ Variab (Normal (OfMiniK e v))
        , OfKTerm e v ]
    => Eq (OfMiniK e v)
deriving stock instance
    With Ord
        [ Variab (Normal (OfMiniK e v))
        , OfKTerm e v ]
    => Ord (OfMiniK e v)
deriving stock instance
    With Generic
        [ Variab (Normal (OfMiniK e v))
        , OfKTerm e v ]
    => Generic (OfMiniK e v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ Variab (Normal (OfMiniK e v))
        , OfKTerm e v ]
    => NFData (OfMiniK e v)

type MiniK = Variab (Normal (OfMiniK Value Concrete))
type MiniKVar = Variab (Normal (OfMiniK Redex Variable))
type MiniKVarValue = Variab (Normal (OfMiniK Value Variable))

data OfKTerm (e :: Evaluated) (v :: Variability)
    = KInt !(VariabOf v OfIntType)
    | KBool !(VariabOf v OfBoolType)
    | KMap (VariabOf v (OfMapType Redex))
    | KExp !(Variab (Normal (OfMiniK e v)))

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

type KTerm = Variab (Normal (OfKTerm Value Concrete))
type KTermVar = Variab (Normal (OfKTerm Redex Variable))
type KTermVarValue = Variab (Normal (OfKTerm Value Variable))

-- data KType
--     = KTypeId
--     | KTypeInt
--     | KTypeBool
--     | KTypeMap

-- data KTerm :: KType -> Type where
--     Id :: !Name -> KTerm KTypeId
--     Ref :: !(KTerm KTypeId) -> KTerm KTypeInt
--     I :: !Int -> KTerm KTypeInt
--     Plus :: !(KTerm KTypeInt) -> !(KTerm KTypeInt) -> KTerm KTypeInt
--     Mod :: !(KTerm KTypeInt) -> !(KTerm KTypeInt) -> KTerm KTypeInt
--     B :: !Bool -> KTerm KTypeBool
--     Not :: !(KTerm KTypeBool) -> KTerm KTypeBool
--     And :: !(KTerm KTypeBool) -> !(KTerm KTypeBool) -> KTerm KTypeBool
--     LT' :: !(KTerm KTypeInt) -> !(KTerm KTypeInt) -> KTerm KTypeBool
--     KMap :: !MapType -> KTerm KTypeMap
--     deriving stock (Show, Eq, Ord, Generic)
--     deriving anyclass NFData

normalizeK
    -- :: Thunk Concrete
    :: Variab (Normal (OfMiniK Redex Concrete))
    -> Variab (Normal (OfMiniK Value Concrete))
normalizeK term = normalizeK' term  KEmpty
  where
    normalizeK' (KSeq term1 term2) normalized = normalizeK' term1
        $ normalizeK' term2 normalized
    normalizeK' (KVal KEmpty) normalized = normalized
    normalizeK' (KVal (KSymbol name body tail)) normalized = KSymbol name body $ normalizeK' tail normalized

-- normalizeK :: MiniK -> [MiniK]
-- normalizeK term = normalizeK' term []
--   where
--   normalizeK' (KSeq term1 term2) normalized = normalizeK' term1
--       $ normalizeK' term2 normalized
--   normalizeK' KEmpty normalized = normalized
--   normalizeK' term1 normalized = term1:normalized

-- deNormalizeK :: [MiniK] -> MiniK
-- deNormalizeK [] = KEmpty
-- deNormalizeK [term, term2] = KSeq term term2
-- deNormalizeK (term:terms) = KSeq term $ deNormalizeK terms

-- reNormalizeK :: MiniK -> MiniK
-- reNormalizeK = deNormalizeK . normalizeK

--

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
type BoolTypeVarValue = Variab (NormalOf Value (OfBoolType Variable))

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

type MapType = Variab (OfMapType Value Concrete)
type MapTypeVar = Variab (OfMapType Redex Variable)

-- retractConcreteMap :: MapType -> [(Name, IntType)]
-- retractConcreteMap (MapVar _) =
--     error "Expecting concrete element of type Map."
-- retractConcreteMap MapEmpty = []
-- retractConcreteMap (MapCons idTerm intTerm mapTerm) =
--     (retractConcreteId idTerm, intTerm)
--     : retractConcreteMap mapTerm

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
data OfKonfiguration (e :: Evaluated) (v :: Variability) =
    Konfiguration
        { k :: !(Variab (Normal (OfMiniK e v)))
        , kState :: !(Variab (OfMapType Value v))
        }

deriving stock instance
    With Show
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType Value v) ]
    => Show (OfKonfiguration e v)
deriving stock instance
    With Eq
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType Value v) ]
    => Eq (OfKonfiguration e v)
deriving stock instance
    With Ord
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType Value v) ]
    => Ord (OfKonfiguration e v)
deriving stock instance
    With Generic
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType Value v) ]
    => Generic (OfKonfiguration e v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ Variab (Normal (OfMiniK e v))
        , Variab (OfMapType Value v) ]
    => NFData (OfKonfiguration e v)

type Konfiguration = OfKonfiguration Value Concrete
type KonfigurationVar = OfKonfiguration Redex Variable
type KonfigurationVarValue = OfKonfiguration Value Variable

-- Can the program be rewritten further?
canBeRewritten :: Konfiguration -> Bool
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

rewrite :: RewriteRule -> RewriteVar
rewrite (RewriteRule _ r c) =
    Rewrite (Konfiguration { k = normalizeK $ k r, kState = kState r }) c

data OfRewrite (v :: Variability) = Rewrite
    { konf :: !(OfKonfiguration Value v)
    , condition :: !(VariabOf v OfBoolType)
    }

deriving stock instance
    With Show
        [ OfKonfiguration Value v
        , VariabOf v OfBoolType ]
    => Show (OfRewrite v)
deriving stock instance
    With Eq
        [ OfKonfiguration Value v
        , VariabOf v OfBoolType ]
    => Eq (OfRewrite v)
deriving stock instance
    With Ord
        [ OfKonfiguration Value v
        , VariabOf v OfBoolType ]
    => Ord (OfRewrite v)
deriving stock instance
    With Generic
        [ OfKonfiguration Value v
        , VariabOf v OfBoolType ]
    => Generic (OfRewrite v)
deriving anyclass instance
    WithAll [Generic, NFData]
        [ OfKonfiguration Value v
        , VariabOf v OfBoolType ]
    => NFData (OfRewrite v)

type Rewrite = OfRewrite Concrete
type RewriteVar = OfRewrite Variable

-- auxiliary machinery

type With :: forall typ. (typ -> Constraint) -> [typ] -> Constraint
type family With constraint types where
    With _ '[] = ()
    With constraint (typ : rest) = (constraint typ, With constraint rest)

type WithAll :: forall typ. [typ -> Constraint] -> [typ] -> Constraint
type family WithAll constraints types where
    WithAll typ '[] = ()
    WithAll (constraint : rest) types = (With constraint types, WithAll rest types)
