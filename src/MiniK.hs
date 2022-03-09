module MiniK
    ( MiniK (..)
    , IdType (..)
    , IntType (..)
    , BoolType (..)
    , MapType (..)
    , Konfiguration (..)
    , RewriteRule (..)
    , Name
    , retractIntTerm
    , retractBoolTerm
    , retractIdTerm
    , retractMapTerm
    , retractConcreteId
    , retractConcreteMap
    , extractConcreteId
    , canBeRewritten
    , normalizeK
    , deNormalizeK
    , reNormalizeK
    ) where

import Control.DeepSeq (type NFData)
import GHC.Generics (type Generic)

type Name = String

-- The AST of the MiniK language for describing computations of
-- user-defined programming languages.
-- An overview of the constructors:
--   - KSeq and KEmpty allow sequencing computations
--   - KSymbol allows defining program constructs with arguments,
--   identified by the name of the symbol
--   - KInt, KBool, KMap are the only types allowed in programs
--   - KVar are variables which represent programs themselves
data KSeqP m
    = KEmpty
    -- | name, body, tail
    | KSymbol !Name ![m (KTermP m)] (m (KSeqP m))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

data KTermP m
    = KExpr !(m (KExprP m))
    | KSeq !(m (KSeqP m))

data KExprP m
    = KInt !(m (IntType m))
    | KBool !(m (BoolType m))
    | KMap (m (MapType m))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

type KMiniK = KSeqP Id
type KMiniKVar = Var (KSeqP Var)

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

data Var a
    = KVar !Name
    | Concrete a


normalizeK :: MiniK -> [MiniK]
normalizeK term = normalizeK' term []
  where
  normalizeK' (KSeq term1 term2) normalized = normalizeK' term1
      $ normalizeK' term2 normalized
  normalizeK' KEmpty normalized = normalized
  normalizeK' term1 normalized = term1:normalized

deNormalizeK :: [MiniK] -> MiniK
deNormalizeK [] = KEmpty
deNormalizeK [term, term2] = KSeq term term2
deNormalizeK (term:terms) = KSeq term $ deNormalizeK terms

reNormalizeK :: MiniK -> MiniK
reNormalizeK = deNormalizeK . normalizeK

--

retractIntTerm :: MiniK -> IntType
retractIntTerm (KInt intTerm) = intTerm
retractIntTerm _ = error "Expecting element of type Int."

retractBoolTerm :: MiniK -> BoolType
retractBoolTerm (KBool boolTerm) = boolTerm
retractBoolTerm _ = error "Expecting element of type Bool."

retractIdTerm :: MiniK -> IdType
retractIdTerm (KInt (IntId idTerm)) = idTerm
retractIdTerm _ = error "Expecting element of type Id."

retractMapTerm :: MiniK -> MapType
retractMapTerm (KMap mapTerm) = mapTerm
retractMapTerm _ = error "Expecting element of type Map."

extractConcreteId :: MiniK -> Maybe Name
extractConcreteId (KInt (IntId (Id name))) = Just name
extractConcreteId _ = Nothing

-- A type for identifiers inside languages defined in MiniK.
-- For simplicity, languages may only have 'IntType' identifiers.
newtype IdType = Id { retractConcreteId :: !Name }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

data IntTypeP m
    = I !Int
    | Ref !(m IdType)
    | Plus !(m IntType) !(m IntType)
    | Mod !(m IntType) !(m IntType)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

type IntType = IntTypeP Id
type IntTypeVar = Var (IntTypeP Var)

data BoolTypeP m
    = B !Bool
    | Not !(m BoolType)
    | And !(m BoolType) !(m BoolType)
    | LT' !(m IntType) !(m IntType)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

type BoolType = BoolTypeP Id
type BoolTypeVar = Var (BoolTypeP Var)

-- A type for MiniK maps, used for storing the values identifiers
-- point to during the execution of a language defined in MiniK.
-- For simplicity, these values are restricted to 'IntType'.
data MapTypeP m
    = MapEmpty
    | MapCons !(m IdType) !(m IntType) !(m MapType)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

type MapType = MapTypeP Id
type MapTypeVar = Var (MapType Var)

retractConcreteMap :: MapType -> [(Name, IntType)]
retractConcreteMap (MapVar _) =
    error "Expecting concrete element of type Map."
retractConcreteMap MapEmpty = []
retractConcreteMap (MapCons idTerm intTerm mapTerm) =
    (retractConcreteId idTerm, intTerm)
    : retractConcreteMap mapTerm

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
data KonfigurationP m =
    Konfiguration
        { k :: !(m MiniK)
        , kState :: !(m MapType)
        }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

type Konfiguration = Konfiguration Id
type KonfigurationVar = Konfiguration Var

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
        { left :: !Konfiguration
        , right :: !KonfigurationVar
        , sideCondition :: !BoolType
        }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData
