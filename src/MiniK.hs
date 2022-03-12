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
import Data.Kind (Type)
import GHC.Generics (type Generic)

type Name = String

type IntValue = Int

data Variability = Variable | Concrete
     deriving stock (Show, Eq, Ord, Generic)
     deriving anyclass NFData

type family Of (v :: Variability) (t :: Variability -> Type) where
  Of Concrete t = t Concrete
  Of Variable t = Var t

data Var (t :: Variability -> Type)
    = KVar !Name
    | K (t Variable)
    | KAll (t Concrete)

deriving stock instance (Show (a Variable), Show (a Concrete))  => Show (Var a)
deriving stock instance (Eq (a Variable), Eq (a Concrete)) => Eq (Var a)
deriving stock instance (Ord (a Variable), Ord (a Concrete)) => Ord (Var a)
deriving stock instance (Generic (a Variable), Generic (a Concrete)) => Generic (Var a)
deriving anyclass instance (Generic (a Variable), Generic (a Concrete), NFData (a Variable), NFData (a Concrete)) => NFData (Var a)

data Evaluated = Redex | Value
     deriving stock (Show, Eq, Ord, Generic)
     deriving anyclass NFData

type family Normal (v :: Evaluated) (t :: Type)
type instance Normal _ (OfIdType t) = OfIdType t
type instance Normal _ (Var t) = Var t
type instance Normal Redex (OfIntType t) = OfIntType t
type instance Normal Value (OfIntType Concrete) = IntValue

-- The AST of the MiniK language for describing computations of
-- user-defined programming languages.
-- An overview of the constructors:
--   - KSeq and KEmpty allow sequencing computations
--   - KSymbol allows defining program constructs with arguments,
--   identified by the name of the symbol
--   - KInt, KBool, KMap are the only types allowed in programs
--   - KVar are variables which represent programs themselves
data OfMiniK (v :: Variability)
    = KEmpty
    -- | name, body, tail
    | KSymbol !Name ![Of v OfKTerm] (Of v OfMiniK)

deriving stock instance (Show (Of m OfMiniK), Show (Of m OfKTerm)) => Show (OfMiniK m)
deriving stock instance (Eq (Of m OfMiniK), Eq (Of m OfKTerm)) => Eq (OfMiniK m)
deriving stock instance (Ord (Of m OfMiniK), Ord (Of m OfKTerm)) => Ord (OfMiniK m)
deriving stock instance (Generic (Of m OfMiniK), Generic (Of m OfKTerm)) => Generic (OfMiniK m)
deriving anyclass instance (Generic (Of m OfMiniK), Generic (Of m OfKTerm), NFData (Of m OfMiniK), NFData (Of m OfKTerm)) => NFData (OfMiniK m)

data OfKTerm (v :: Variability)
    = KInt !(Of v OfIntType)
    | KBool !(Of v OfBoolType)
    | KMap (Of v (OfMapType Redex))
    | KSeq !(Of v OfMiniK)

deriving stock instance (Show (Of m OfIntType), Show (Of m OfBoolType), Show (Of m (OfMapType Redex)), Show (Of m OfMiniK)) => Show (OfKTerm m)
deriving stock instance (Eq (Of m OfIntType), Eq (Of m OfBoolType), Eq (Of m (OfMapType Redex)), Eq (Of m OfMiniK)) => Eq (OfKTerm m)
deriving stock instance (Ord (Of m OfIntType), Ord (Of m OfBoolType), Ord (Of m (OfMapType Redex)), Ord (Of m OfMiniK)) => Ord (OfKTerm m)
deriving stock instance (Generic (Of m OfIntType), Generic (Of m OfBoolType), Generic (Of m (OfMapType Redex)), Generic (Of m OfMiniK)) => Generic (OfKTerm m)
deriving anyclass instance (Generic (Of m OfIntType), Generic (Of m OfBoolType), Generic (Of m (OfMapType Redex)), Generic (Of m OfMiniK), NFData (Of m OfIntType), NFData (Of m OfBoolType), NFData (Of m (OfMapType Redex)), NFData (Of m OfMiniK)) => NFData (OfKTerm m)

type KMiniK = Of Concrete OfMiniK
type KMiniKVar = Of Variable OfMiniK

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


-- normalizeK :: MiniK -> [MiniK], Generic

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

-- retractIntTerm :: MiniK -> IntType
-- retractIntTerm (KInt intTerm) = intTerm
-- retractIntTerm _ = error "Expecting element of type Int."

-- retractBoolTerm :: MiniK -> BoolType
-- retractBoolTerm (KBool boolTerm) = boolTerm
-- retractBoolTerm _ = error "Expecting element of type Bool."

-- retractIdTerm :: MiniK -> IdType
-- retractIdTerm (KInt (IntId idTerm)) = idTerm
-- retractIdTerm _ = error "Expecting element of type Id."

-- retractMapTerm :: MiniK -> MapType
-- retractMapTerm (KMap mapTerm) = mapTerm
-- retractMapTerm _ = error "Expecting element of type Map."

-- extractConcreteId :: MiniK -> Maybe Name
-- extractConcreteId (KInt (IntId (Id name))) = Just name
-- extractConcreteId _ = Nothing

-- A type for identifiers inside languages defined in MiniK.
-- For simplicity, languages may only have 'IntType' identifiers.
newtype OfIdType (v :: Variability) = Id { retractConcreteId :: Name }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

type IdType = OfIdType Concrete

data OfIntType (v :: Variability)
    = I !IntValue
    | Ref !(Of v OfIdType)
    | Plus !(Of v OfIntType) !(Of v OfIntType)
    | Mod !(Of v OfIntType) !(Of v OfIntType)

deriving stock instance (Show (Of v OfIntType), Show (Of v OfIdType)) => Show (OfIntType v)
deriving stock instance (Eq (Of v OfIntType), Eq (Of v OfIdType)) => Eq (OfIntType v)
deriving stock instance (Ord (Of v OfIntType), Ord (Of v OfIdType)) => Ord (OfIntType v)
deriving stock instance (Generic (Of v OfIntType), Generic (Of v OfIdType)) => Generic (OfIntType v)
deriving anyclass instance (Generic (Of v OfIntType), Generic (Of v OfIdType), NFData (Of v OfIntType), NFData (Of v OfIdType)) => NFData (OfIntType v)

type IntType = Of Concrete OfIntType
type IntTypeVar = Of Variable OfIntType

data OfBoolType (v :: Variability)
    = B !Bool
    | Not !(Of v OfBoolType)
    | And !(Of v OfBoolType) !(Of v OfBoolType)
    | LT' !(Of v OfIntType) !(Of v OfIntType)

deriving stock instance (Show (Of v OfBoolType), Show (Of v OfIntType)) => Show (OfBoolType v)
deriving stock instance (Eq (Of v OfBoolType), Eq (Of v OfIntType)) => Eq (OfBoolType v)
deriving stock instance (Ord (Of v OfBoolType), Ord (Of v OfIntType)) => Ord (OfBoolType v)
deriving stock instance (Generic (Of v OfBoolType), Generic (Of v OfIntType)) => Generic (OfBoolType v)
deriving anyclass instance (Generic (Of v OfBoolType), Generic (Of v OfIntType), NFData (Of v OfBoolType), NFData (Of v OfIntType)) => NFData (OfBoolType v)

type BoolType = Of Concrete OfBoolType
type BoolTypeVar = Of Variable OfBoolType

-- A type for MiniK maps, used for storing the values identifiers
-- point to during the execution of a language defined in MiniK.
-- For simplicity, these values are restricted to 'IntType'.
data OfMapType (e :: Evaluated) (v :: Variability)
    = MapEmpty
    | MapCons !(Normal e (Of v OfIdType)) !(Normal e (Of v OfIntType)) !(Of v (OfMapType e))

deriving stock instance (Show (Normal e (Of v OfIdType)), Show (Normal e (Of v OfIntType)), Show (Of v (OfMapType e))) => Show (OfMapType e v)
deriving stock instance (Eq (Normal e (Of v OfIdType)), Eq (Normal e (Of v OfIntType)), Eq (Of v (OfMapType e))) => Eq (OfMapType e v)
deriving stock instance (Ord (Normal e (Of v OfIdType)), Ord (Normal e (Of v OfIntType)), Ord (Of v (OfMapType e))) => Ord (OfMapType e v)
deriving stock instance (Generic (Normal e (Of v OfIdType)), Generic (Normal e (Of v OfIntType)), Generic (Of v (OfMapType e))) => Generic (OfMapType e v)
deriving anyclass instance (Generic (Normal e (Of v OfIdType)), Generic (Normal e (Of v OfIntType)), Generic (Of v (OfMapType e)), NFData (Normal e (Of v OfIdType)), NFData (Normal e (Of v OfIntType)), NFData (Of v (OfMapType e))) => NFData (OfMapType e v)

type MapType = Of Concrete (OfMapType Value)
type MapTypeVar = Of Variable (OfMapType Redex)

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
        { k :: !(Of v OfMiniK)
        , kState :: !(Of v (OfMapType e))
        }

deriving stock instance (Show (Of v OfMiniK), Show (Of v (OfMapType e))) => Show (OfKonfiguration e v)
deriving stock instance (Eq (Of v OfMiniK), Eq (Of v (OfMapType e))) => Eq (OfKonfiguration e v)
deriving stock instance (Ord (Of v OfMiniK), Ord (Of v (OfMapType e))) => Ord (OfKonfiguration e v)
deriving stock instance (Generic (Of v OfMiniK), Generic (Of v (OfMapType e))) => Generic (OfKonfiguration e v)
deriving anyclass instance (Generic (Of v OfMiniK), Generic (Of v (OfMapType e)), NFData (Of v OfMiniK), NFData (Of v (OfMapType e))) => NFData (OfKonfiguration e v)

type Konfiguration = OfKonfiguration Value Concrete
type KonfigurationVar = OfKonfiguration Redex Variable

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
        { left :: !KonfigurationVar
        , right :: !KonfigurationVar
        , sideCondition :: !BoolType
        }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData
