module Substitution
    ( Substitution
    , SubstitutionVariable (lookup)
    , empty
    , insert
    , union
    , multiUnion
    -- , variablesToSubstitute
    -- , toList
    -- , getIds
    ) where

import MiniK
import Control.DeepSeq (type NFData)
import Data.Map.Lazy (Map)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import GHC.Generics (type Generic)

-- A substitution is an association between variables and
-- MiniK terms.
data Substitution = Substitution
    { varK :: Map Name MiniK
    , varId :: Map Name IdType
    , varInt :: Map Name IntType
    , varBool :: Map Name BoolType
    , varMap :: Map Name MapType
    }
    deriving stock (Show, Generic)
    deriving anyclass NFData

empty :: Substitution
empty = Substitution Map.empty

insert :: Name -> MiniK -> Substitution -> Substitution
insert name kTerm (Substitution subst) =
    Substitution
    $ Map.insert name kTerm subst

union :: Substitution -> Substitution -> Substitution
union (Substitution subst1) (Substitution subst2) =
    Substitution
    $ Map.union subst1 subst2

multiUnion :: [Substitution] -> Substitution
multiUnion =
    foldl union empty

class SubstitutionVariable v where
    lookup :: Name -> Substitution -> Maybe v

instance SubstitutionVariable MiniK where
    lookup name s = Map.lookup name $ varK s
instance SubstitutionVariable IdType where
    lookup name s = Map.lookup name $ varId s
instance SubstitutionVariable IntType where
    lookup name s = Map.lookup name $ varInt s
instance SubstitutionVariable BoolType where
    lookup name s = Map.lookup name $ varBool s
instance SubstitutionVariable MapType where
    lookup name s = Map.lookup name $ varMap s

-- variablesToSubstitute :: Substitution -> [Name]
-- variablesToSubstitute (Substitution subst) =
--     Map.keys subst

-- toList :: Substitution -> [(Name, MiniK)]
-- toList (Substitution subst) =
--     Map.toList subst

-- getIds :: Substitution -> Set Name
-- getIds (Substitution subst) =
--     let
--       k $ subst
--       kId $ subst
--       kInt $ subst
--       kMap $ subst in
--     let values = mapMaybe extractConcreteId . Map.elems $ subst
--      in Set.fromList values
--   where
--     extractConcreteId :: MiniK -> Maybe Name
--     extractConcreteId (KInt (IntId (Id name))) = Just name
--     extractConcreteId _ = Nothing
