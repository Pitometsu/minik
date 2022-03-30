module Substitution
    ( Substitution
    , SubstitutionElement (lookup, insert)
    , empty
    , union
    , multiUnion
    , getIds
    ) where

import MiniK
import Control.DeepSeq (type NFData)
import Data.Kind (type Type)
import Data.Map.Lazy (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map.Lazy qualified as Map
import GHC.Generics (type Generic)

-- A substitution is an association between variables and
-- MiniK terms.
data Substitution = Substitution
    { varK :: !(Map Name (OfMiniK Value Concrete))
    , varId :: !(Map Name IdType)
    , varInt :: !(Map Name IntType)
    , varBool :: !(Map Name BoolType)
    , varMap :: !(Map Name MapTypeRedex)
    }
    deriving stock (Show, Generic)
    deriving anyclass NFData

empty :: Substitution
empty = Substitution Map.empty Map.empty Map.empty Map.empty Map.empty

union :: Substitution -> Substitution -> Substitution
union subst1 subst2 =
    Substitution
        { varK = Map.union (varK subst1) $ varK subst2
        , varId = Map.union (varId subst1) $ varId subst2
        , varInt = Map.union (varInt subst1) $ varInt subst2
        , varBool = Map.union (varBool subst1) $ varBool subst2
        , varMap = Map.union (varMap subst1) $ varMap subst2
        }

multiUnion :: [Substitution] -> Substitution
multiUnion =
    foldl union empty

class SubstitutionElement (term :: Variability -> Type) where
    lookup :: Name -> Substitution -> Maybe (term Concrete)
    insert :: Name -> term Concrete -> Substitution -> Substitution

instance SubstitutionElement (OfMiniK Value) where
    lookup name subst = Map.lookup name $ varK subst
    insert name term subst =
        subst { varK = Map.insert name term $ varK subst }
instance SubstitutionElement OfIdType where
    lookup name subst = Map.lookup name $ varId subst
    insert name term subst =
        subst { varId = Map.insert name term $ varId subst }
instance SubstitutionElement OfIntType where
    lookup name subst = Map.lookup name $ varInt subst
    insert name term subst =
        subst { varInt = Map.insert name term $ varInt subst }
instance SubstitutionElement OfBoolType where
    lookup name subst = Map.lookup name $ varBool subst
    insert name term subst =
        subst { varBool = Map.insert name term $ varBool subst }
instance SubstitutionElement (OfMapType Redex) where
    lookup name subst = Map.lookup name $ varMap subst
    insert name term subst =
        subst { varMap = Map.insert name term $ varMap subst }

getIds :: Substitution -> Set Name
getIds Substitution { varId } = Set.fromList $ retractConcreteId <$> Map.elems varId
