module NormalizedMap
    ( OfNormalizedMap (..)
    , OfNormalized (..)
    , OfMap (..)
    , MapOf (..)
    , MapOfInt
    , NormalizedMapOf (..)
    , NormalizedOf (..)
    , ConcreteMapRedex
    , NormalizedMapConcrete (..)
    , normalizeVar
    , renormalizeTraverse
    ) where

import Prelude hiding (map, traverse)
import Prelude qualified as P (traverse)
import MiniK
import Control.DeepSeq (type NFData)
import Data.Kind (type Type)
import Data.Function ((&))
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import GHC.Generics (type Generic)

-- A normalized representation of program configuration maps.
--
-- The opaque component represents a possible map variable.
--
-- The symbolic component is the part of the map where the
-- keys are variables, identified by their names.
--
-- The concrete component is the part of the map where the
-- keys are identifiers, which we recognize by their names.

newtype OfNormalizedMap (v :: Variability) (e :: Evaluated) = NormalizedMap
    { unNormalizedMap :: OfNormalized v e (Variab (NormalOf e (OfIntType v))) }

deriving stock instance
    Show (Variab (NormalOf e (OfIntType v))) => Show (OfNormalizedMap v e)
deriving stock instance
    Eq (Variab (NormalOf e (OfIntType v))) => Eq (OfNormalizedMap v e)
deriving stock instance
   Ord (Variab (NormalOf e (OfIntType v))) => Ord (OfNormalizedMap v e)
deriving stock instance
    Generic (Variab (NormalOf e (OfIntType v))) => Generic (OfNormalizedMap v e)
deriving anyclass instance
    WithAll [Generic, NFData]
        '[ Variab (NormalOf e (OfIntType v)) ]
    => NFData (OfNormalizedMap v e)

-- OfNormalizedMap Concrete Value -- ^ for configuration state
-- OfNormalizedMap Variable Redex -- ^ for Map in MiniK expession
-- OfNormalizedMap Variable Value -- ^ variable state
-- OfNormalizedMap Concrete Redex -- ^ state after subst, till eval

data OfNormalized (v :: Variability) (e :: Evaluated) (t :: Type) = OfNormalized
    { opaque :: !(Maybe Name)
    , symbolic :: !(MapOf v e t)
    , concrete :: !(MapOf v e t)
    }
    deriving (Functor, Foldable, Traversable)
        via (OfNormalized v e)

deriving stock instance Show t => Show (OfNormalized v e t)
deriving stock instance Eq t => Eq (OfNormalized v e t)
deriving stock instance Ord t => Ord (OfNormalized v e t)
deriving stock instance Generic t => Generic (OfNormalized v e t)
deriving anyclass instance
    WithAll [Generic, NFData] '[ t ] => NFData (OfNormalized v e t)

newtype OfMap (v :: Variability) (e :: Evaluated)
    = OfMap { unOfMap :: MapOfInt v e }

deriving stock instance
    Show (Variab (NormalOf e (OfIntType v))) => Show (OfMap v e)
deriving stock instance
    Eq (Variab (NormalOf e (OfIntType v))) => Eq (OfMap v e)
deriving stock instance
   Ord (Variab (NormalOf e (OfIntType v))) => Ord (OfMap v e)
deriving stock instance
    Generic (Variab (NormalOf e (OfIntType v))) => Generic (OfMap v e)
deriving anyclass instance
    WithAll [Generic, NFData]
        '[ Variab (NormalOf e (OfIntType v)) ]
    => NFData (OfMap v e)

newtype MapOf (v :: Variability) (e :: Evaluated) (t :: Type)
    = MapOf { unMapOf :: Map Name t }
    deriving (Functor, Applicative, Monad, Foldable, Traversable)
        via (MapOf v e)

deriving stock instance Show t => Show (MapOf v e t)
deriving stock instance Eq t => Eq (MapOf v e t)
deriving stock instance Ord t => Ord (MapOf v e t)
deriving stock instance Generic t => Generic (MapOf v e t)
deriving anyclass instance
    WithAll [Generic, NFData] '[ t ] => NFData (MapOf v e t)

type MapOfInt (v :: Variability) (e :: Evaluated)
    = MapOf v e (Variab (NormalOf e (OfIntType v)))

type ConcreteMapRedex = OfMap Concrete Redex

-- wrapper for Match type class instance

newtype NormalizedMapOf (e :: Evaluated) (v :: Variability) = NormalizedMapOf
    { unNormalizedMapOf :: NormalizedOf e v (ConcreteMapOf v e) }

deriving stock instance
    Show (ConcreteMapOf v e) => Show (NormalizedMapOf e v)
deriving stock instance
    Eq (ConcreteMapOf v e) => Eq (NormalizedMapOf e v)
deriving stock instance
   Ord (ConcreteMapOf v e) => Ord (NormalizedMapOf e v)
deriving stock instance
    Generic (ConcreteMapOf v e) => Generic (NormalizedMapOf e v)
deriving anyclass instance
    WithAll [Generic, NFData]
        '[ ConcreteMapOf v e ]
    => NFData (NormalizedMapOf e v)

newtype NormalizedOf (e :: Evaluated) (v :: Variability) (t :: Type)
    = NormalizedOf { unNormalizedOf :: t }
    deriving (Functor, Foldable, Traversable)
        via (NormalizedOf e v)

deriving stock instance Show t => Show (NormalizedOf e v t)
deriving stock instance Eq t => Eq (NormalizedOf e v t)
deriving stock instance Ord t => Ord (NormalizedOf e v t)
deriving stock instance Generic t => Generic (NormalizedOf e v t)
deriving anyclass instance
    WithAll [Generic, NFData] '[ t ] => NFData (NormalizedOf e v t)

class WithAll [Show, Eq, Ord, Generic, NFData]
        '[ ConcreteMapOf v Redex, ConcreteMapOf v Value ]
    => NormalizedMapConcrete (v :: Variability)
  where
    type ConcreteMapOf v :: Evaluated -> Type

    map :: forall (e :: Evaluated)
            (e' :: Evaluated)
        . (Variab (NormalOf e (OfIntType v))
            -> Variab (NormalOf e' (OfIntType v)))
        -> ConcreteMapOf v e
        -> ConcreteMapOf v e'

    traverse :: forall (e :: Evaluated)
            (e' :: Evaluated)
            (m :: Type -> Type)
        . Applicative m
        => (Variab (NormalOf e (OfIntType v))
            -> m (Variab (NormalOf e' (OfIntType v))))
        -> ConcreteMapOf v e
        -> m (ConcreteMapOf v e')

    empty
        :: forall (e :: Evaluated)
        . ConcreteMapOf v e

    isEmpty
        :: forall (e :: Evaluated)
        . ConcreteMapOf v e
        -> Bool

    union
        :: forall (e :: Evaluated)
        . ConcreteMapOf v e
        -> ConcreteMapOf v e
        -> ConcreteMapOf v e

    normalize
        :: forall (e :: Evaluated)
        . OfMapType e v
        -> ConcreteMapOf v e

    unNormalize
        :: forall (e :: Evaluated)
        . ConcreteMapOf v e
        -> Variab (OfMapType e v)

    lookupConcreteId
        :: forall (e :: Evaluated)
        . Name
        -> ConcreteMapOf v e
        -> Maybe (Variab (NormalOf e (OfIntType v)))

instance NormalizedMapConcrete Variable where
    type ConcreteMapOf Variable = OfNormalizedMap Variable

    map :: forall (e :: Evaluated)
            (e' :: Evaluated)
        . (Variab (NormalOf e (OfIntType Variable))
            -> Variab (NormalOf e' (OfIntType Variable)))
        -> ConcreteMapOf Variable e
        -> ConcreteMapOf Variable e'
    map f
        (NormalizedMap OfNormalized
            { opaque
            , symbolic
            , concrete
            })
      =
        NormalizedMap OfNormalized
            { opaque
            , symbolic = map' @e @e' @Variable f symbolic
            , concrete = map' @e @e' @Variable f concrete
            }

    traverse f
        (NormalizedMap OfNormalized
            { opaque
            , symbolic
            , concrete
            })
      = do
          symbolic' <- traverse' f symbolic
          concrete' <- traverse' f concrete
          pure $ NormalizedMap OfNormalized
              { opaque
              , symbolic = symbolic'
              , concrete = concrete'
              }

    empty
        :: forall (e :: Evaluated)
        . ConcreteMapOf Variable e
    empty =
        NormalizedMap OfNormalized
            { opaque = Nothing
            , symbolic = empty' @e @Variable
            , concrete = empty' @e @Variable
            }

    isEmpty
        :: forall (e :: Evaluated)
        . ConcreteMapOf Variable e
        -> Bool
    isEmpty
        (NormalizedMap OfNormalized
            { opaque
            , symbolic
            , concrete
            })
      =
        null opaque
        && isEmpty' @e @Variable symbolic
        && isEmpty' @e @Variable concrete

    union
        :: forall (e :: Evaluated)
        . ConcreteMapOf Variable e
        -> ConcreteMapOf Variable e
        -> ConcreteMapOf Variable e
    union
        (NormalizedMap OfNormalized
            { opaque = opaque1
            , symbolic = symbolic1
            , concrete = concrete1
            })
        (NormalizedMap OfNormalized
            { opaque = opaque2
            , symbolic = symbolic2
            , concrete = concrete2
            })
      =
        NormalizedMap OfNormalized
            { opaque =
                opaque1 <> opaque2
            , symbolic =
                union' @e @Variable symbolic1 symbolic2
            , concrete =
                union' @e @Variable concrete1 concrete2
            }

    normalize MapEmpty = empty @Variable
    normalize (MapCons idTerm intTerm mapTerm) = union @Variable
        (empty @Variable & case idTerm of
            KVar name
                -> \(NormalizedMap nMap) -> NormalizedMap nMap
                    { symbolic = MapOf $ Map.singleton name intTerm }
            K (Id name)
                -> \(NormalizedMap nMap) -> NormalizedMap nMap
                    { concrete = MapOf $ Map.singleton name intTerm })
        $ normalizeVar mapTerm

    unNormalize
        nMap@(NormalizedMap OfNormalized
            { opaque
            , symbolic = MapOf symbolic
            , concrete = MapOf concrete
            })
        | isEmpty @Variable nMap = K MapEmpty
        | otherwise =
            let concrete' =
                    Map.mapKeys (K . Id) concrete
                    & Map.toList
                symbolic' =
                    Map.mapKeys (KVar @OfIdType) symbolic
                    & Map.toList
                elements = symbolic' <> concrete'
                cons' (id', elem') rest = K $ MapCons id' elem' rest
            in
                foldr cons' (maybe (K MapEmpty) KVar opaque) elements

    lookupConcreteId name (NormalizedMap OfNormalized { concrete }) =
        lookupConcreteId' name concrete

instance NormalizedMapConcrete Concrete where
    type ConcreteMapOf Concrete = OfMap Concrete

    map :: forall (e :: Evaluated)
            (e' :: Evaluated)
        . (Variab (NormalOf e (OfIntType Concrete))
            -> Variab (NormalOf e' (OfIntType Concrete)))
        -> ConcreteMapOf Concrete e
        -> ConcreteMapOf Concrete e'
    map f = OfMap . map' @e @e' @Concrete f . unOfMap

    traverse f = fmap OfMap . traverse' f . unOfMap

    empty
        :: forall (e :: Evaluated)
        . ConcreteMapOf Concrete e
    empty = OfMap $ empty' @e @Concrete

    isEmpty
        :: forall (e :: Evaluated)
        . ConcreteMapOf Concrete e
        -> Bool
    isEmpty = isEmpty' @e @Concrete . unOfMap

    union
        :: forall (e :: Evaluated)
        . ConcreteMapOf Concrete e
        -> ConcreteMapOf Concrete e
        -> ConcreteMapOf Concrete e
    union x y = OfMap . union' @e @Concrete (unOfMap x) $ unOfMap y

    normalize MapEmpty = empty @Concrete
    normalize (MapCons (Id name) intTerm mapTerm) =
        union @Concrete (OfMap . MapOf $ Map.singleton name intTerm)
        $ normalize mapTerm

    unNormalize (OfMap (MapOf cMap))
        | Map.null cMap = MapEmpty
        | otherwise =
            let
                elements = Map.mapKeys Id cMap & Map.toList
            in
                foldr cons' MapEmpty elements
      where
        cons' (id', elem') rest = MapCons id' elem' rest

    lookupConcreteId cId = lookupConcreteId' cId . unOfMap

-- helpers

map'
    :: forall (e :: Evaluated)
        (e' :: Evaluated)
        (v :: Variability)
    . (Variab (NormalOf e (OfIntType v))
        -> Variab (NormalOf e' (OfIntType v)))
    -> MapOfInt v e
    -> MapOfInt v e'
map' f = MapOf . fmap f . unMapOf

traverse' :: forall (e :: Evaluated)
        (e' :: Evaluated)
        (v :: Variability)
        (m :: Type -> Type)
    . Applicative m
    => (Variab (NormalOf e (OfIntType v))
        -> m (Variab (NormalOf e' (OfIntType v))))
    -> MapOfInt v e
    -> m (MapOfInt v e')
traverse' f = fmap MapOf . P.traverse f . unMapOf

empty'
    :: forall (e :: Evaluated)
        (v :: Variability)
    . MapOfInt v e
empty' = MapOf Map.empty

isEmpty'
    :: forall (e :: Evaluated)
        (v :: Variability)
    . MapOfInt v e
    -> Bool
isEmpty' = Map.null . unMapOf

union'
    :: forall (e :: Evaluated)
        (v :: Variability)
    . MapOfInt v e
    -> MapOfInt v e
    -> MapOfInt v e
union' x y = MapOf . Map.union (unMapOf x) $ unMapOf y

lookupConcreteId'
    :: forall (e :: Evaluated)
        (v :: Variability)
    . Name
    -> MapOfInt v e
    -> Maybe (Variab (NormalOf e (OfIntType v)))
lookupConcreteId' name = Map.lookup name . unMapOf

-- util

normalizeVar
    :: forall (e :: Evaluated)
    . Var (OfMapType e)
    -> OfNormalizedMap Variable e
normalizeVar (KVar name) = NormalizedMap (unNormalizedMap $ empty @Variable) { opaque = Just name }
normalizeVar (K term) = normalize term

renormalizeTraverse
    :: forall (e :: Evaluated)
        (e' :: Evaluated)
        (v :: Variability)
        (m :: Type -> Type)
    . (NormalizedMapConcrete v, Applicative m)
    => (Variab (NormalOf e (OfIntType v))
        -> m (Variab (NormalOf e' (OfIntType v))))
    -> OfMapType e v
    -> m (Variab (OfMapType e' v))
renormalizeTraverse f = fmap unNormalize . traverse @v f . normalize
