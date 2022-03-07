module NormalizedMap
    ( NormalizedMap (..)
    , normalize
    , unNormalize
    , renormalize
    , fromConcrete
    , lookupConcreteId
    , map
    ) where

import Prelude hiding (map)
import MiniK
import Data.Map.Lazy (Map)
import Data.Function ((&))
import qualified Data.Map.Lazy as Map

-- A normalized representation of program configuration maps.
--
-- The opaque component represents a possible map variable.
--
-- The symbolic component is the part of the map where the
-- keys are variables, identified by their names.
--
-- The concrete component is the part of the map where the
-- keys are identifiers, which we recognize by their names.
data NormalizedMap =
    NormalizedMap
        { opaque :: !(Maybe Name)
        , symbolic :: !(Map Name IntType)
        , concrete :: !(Map Name IntType)
        }

map :: (IntType -> IntType) -> NormalizedMap -> NormalizedMap
map f
    NormalizedMap
        { opaque
        , symbolic
        , concrete
        }
  =
    NormalizedMap
        { opaque
        , symbolic = f <$> symbolic
        , concrete = f <$> concrete
        }

empty :: NormalizedMap
empty =
    NormalizedMap
        { opaque = Nothing
        , symbolic = Map.empty
        , concrete = Map.empty
        }

isEmpty :: NormalizedMap -> Bool
isEmpty
    NormalizedMap
        { opaque
        , symbolic
        , concrete
        }
  =
    null opaque
    && Map.null symbolic
    && Map.null concrete

fromConcrete :: Map Name IntType -> NormalizedMap
fromConcrete concreteElems =
    empty
        { concrete = concreteElems
        }

union :: NormalizedMap -> NormalizedMap -> NormalizedMap
union
    NormalizedMap
        { opaque = opaque1
        , symbolic = symbolic1
        , concrete = concrete1
        }
    NormalizedMap
        { opaque = opaque2
        , symbolic = symbolic2
        , concrete = concrete2
        }
  =
    NormalizedMap
        { opaque =
            opaque1 <> opaque2
        , symbolic =
            Map.union symbolic1 symbolic2
        , concrete =
            Map.union concrete1 concrete2
        }

normalize :: MapType -> NormalizedMap
normalize (MapVar name) =
    NormalizedMap
        { opaque = Just name
        , symbolic = Map.empty
        , concrete = Map.empty
        }
normalize MapEmpty = empty
normalize (MapCons idTerm intTerm mapTerm) =
    case idTerm of
        IdVar name ->
            NormalizedMap
                { opaque = Nothing
                , symbolic = Map.singleton name intTerm
                , concrete = Map.empty
                }
            `union` normalize mapTerm
        Id name ->
            NormalizedMap
                { opaque = Nothing
                , symbolic = Map.empty
                , concrete = Map.singleton name intTerm
                }
            `union` normalize mapTerm

unNormalize :: NormalizedMap -> MapType
unNormalize
    nMap@NormalizedMap
        { opaque
        , symbolic
        , concrete
        }
    | isEmpty nMap = MapEmpty
    | otherwise =
        let concrete' =
                Map.mapKeys Id concrete
                & Map.toList
            symbolic' =
                Map.mapKeys IdVar symbolic
                & Map.toList
            elements = symbolic' <> concrete'
         in
            case opaque of
                Just varName ->
                    foldr (uncurry MapCons) (MapVar varName) elements
                Nothing ->
                    foldr (uncurry MapCons) MapEmpty elements

renormalize :: MapType -> MapType
renormalize = unNormalize . normalize

lookupConcreteId :: Name -> NormalizedMap -> Maybe IntType
lookupConcreteId name NormalizedMap { concrete } =
    Map.lookup name concrete
