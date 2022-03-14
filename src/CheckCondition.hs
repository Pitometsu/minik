module CheckCondition
    ( evaluate
    , checkCondition
    ) where

import MiniK
import NormalizedMap (NormalizedMap)
import qualified NormalizedMap
import Data.Maybe (fromMaybe)
-- import Debug.Trace

-- Check the side condition of a rule
--
-- This evaluates the bool and int terms,
-- looking up any identifiers from the program state.
checkCondition :: NormalizedMap -> BoolType -> Bool
checkCondition _ (B val) = val
checkCondition mapTerm (Not boolTerm) =
    -- traceEvent "Check condition \"Not\"" $
    not (checkCondition mapTerm boolTerm)
checkCondition mapTerm (And boolTerm1 boolTerm2) =
    -- traceEvent "Check condition \"And\"" $
    checkCondition mapTerm boolTerm1
    && checkCondition mapTerm boolTerm2
checkCondition mapTerm (LT' intTerm1 intTerm2) =
    -- traceEvent "Check condition \"Less than\"" $
    evaluate mapTerm intTerm1
    < evaluate mapTerm intTerm2

evaluate :: NormalizedMap -> IntType -> Int
evaluate _ (I val) = val
evaluate mapTerm (IntId idType) =
    evaluate mapTerm (lookupId idType mapTerm)
evaluate mapTerm (Plus intTerm1 intTerm2) =
    evaluate mapTerm intTerm1
    + evaluate mapTerm intTerm2
evaluate mapTerm (Mod intTerm1 intTerm2) =
    evaluate mapTerm intTerm1
    `mod` evaluate mapTerm intTerm2

lookupId :: IdType -> NormalizedMap -> Maybe IntType
lookupId (Id name) nMap = NormalizedMap.lookupConcreteId name nMap
