module Substitution
    ( Substitution
    , empty
    , insert
    , union
    , multiUnion
    , variablesToSubstitute
    , toList
    , getIds
    ) where

import MiniK
import Data.Map.Lazy (Map)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map

-- A substitution is an association between variables and
-- MiniK terms.
newtype Substitution = Substitution (Map Name MiniK)

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

variablesToSubstitute :: Substitution -> [Name]
variablesToSubstitute (Substitution subst) =
    Map.keys subst

toList :: Substitution -> [(Name, MiniK)]
toList (Substitution subst) =
    Map.toList subst

getIds :: Substitution -> Set Name
getIds (Substitution subst) =
    let values = mapMaybe extractConcreteId . Map.elems $ subst
     in Set.fromList values
