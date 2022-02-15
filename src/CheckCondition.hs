module CheckCondition
    ( checkCondition
    ) where

import MiniK
import NormalizedMap (NormalizedMap)
import qualified NormalizedMap
import Data.Maybe (fromMaybe)
import Debug.Trace

-- Check the side condition of a rule
--
-- This evaluates the bool and int terms,
-- looking up any identifiers from the program state.
checkCondition :: NormalizedMap -> BoolType -> Bool
checkCondition _ (B val) = val
checkCondition _ (BoolVar _) =
    error "Variable should have been instantiated."
checkCondition mapTerm (Not boolTerm) =
    not (checkCondition mapTerm boolTerm)
checkCondition mapTerm (And boolTerm1 boolTerm2) = 
    checkCondition mapTerm boolTerm1
    && checkCondition mapTerm boolTerm2
checkCondition mapTerm (LT' intTerm1 intTerm2) =
    evaluate mapTerm intTerm1
    < evaluate mapTerm intTerm2

evaluate :: NormalizedMap -> IntType -> Int
evaluate _ (I val) = val
evaluate _ (IntVar _) =
    error "Variable should have been instantiated."
evaluate mapTerm (IntId idType) =
    evaluate mapTerm (lookupId idType mapTerm)
evaluate mapTerm (Plus intTerm1 intTerm2) =
    evaluate mapTerm intTerm1
    + evaluate mapTerm intTerm2
evaluate mapTerm (Mod intTerm1 intTerm2) =
    evaluate mapTerm intTerm1
    `mod` evaluate mapTerm intTerm2

lookupId :: IdType -> NormalizedMap -> IntType
lookupId (IdVar _) _ =
    error "Variable should have been instantiated."
lookupId (Id name) nMap =
    fromMaybe
        (error "Tried to lookup undefined identifier.")
        (NormalizedMap.lookupConcreteId name nMap)
