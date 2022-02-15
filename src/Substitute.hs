module Substitute
    ( substitute
    ) where

import Substitution (Substitution)
import qualified Substitution
import MiniK
import qualified Control.Monad as Monad

-- Procedure for applying a substitution to a rewrite rule.
-- It checks that the given substitution contains all the
-- variables inside the rewrite rule, before actually
-- applying the substitution.
substitute :: Substitution -> RewriteRule -> Maybe RewriteRule
substitute subst rule = do
    Monad.guard (coversAllRuleVars subst rule)
    let listOfSubsts = Substitution.toList subst
    return (foldl substituteVariable rule listOfSubsts)

coversAllRuleVars :: Substitution -> RewriteRule -> Bool
coversAllRuleVars subst rule =
    all (`elem` variablesToSubstitute) (extractVariables rule)
  where
    variablesToSubstitute = Substitution.variablesToSubstitute subst

class ExtractVariables term where
    extractVariables :: term -> [Name]

instance ExtractVariables RewriteRule where
    extractVariables RewriteRule { left, right, sideCondition } =
        extractVariables left
        <> extractVariables right
        <> extractVariables sideCondition

instance ExtractVariables Konfiguration where
    extractVariables Konfiguration { k, kState } =
        extractVariables k
        <> extractVariables kState

instance ExtractVariables MiniK where
    extractVariables KEmpty = []
    extractVariables (KInt intTerm) =
        extractVariables intTerm
    extractVariables (KBool boolTerm) =
        extractVariables boolTerm
    extractVariables (KMap mapTerm) =
        extractVariables mapTerm
    extractVariables (KVar name) =
        [name]
    extractVariables (KSymbol _ args) =
        foldMap extractVariables args
    extractVariables (KSeq kTerm1 kTerm2) =
        extractVariables kTerm1
        <> extractVariables kTerm2

instance ExtractVariables IntType where
    extractVariables (I _) = []
    extractVariables (IntVar name) =
        [name]
    extractVariables (IntId idTerm) =
        extractVariables idTerm
    extractVariables (Plus intTerm1 intTerm2) =
        extractVariables intTerm1
        <> extractVariables intTerm2
    extractVariables (Mod intTerm1 intTerm2) =
        extractVariables intTerm1
        <> extractVariables intTerm2

instance ExtractVariables IdType where
    extractVariables (Id _) = []
    extractVariables (IdVar name) =
        [name]

instance ExtractVariables BoolType where
    extractVariables (B _) = []
    extractVariables (BoolVar name) =
        [name]
    extractVariables (Not boolTerm) =
        extractVariables boolTerm
    extractVariables (And boolTerm1 boolTerm2) =
        extractVariables boolTerm1
        <> extractVariables boolTerm2
    extractVariables (LT' intTerm1 intTerm2) =
        extractVariables intTerm1
        <> extractVariables intTerm2

instance ExtractVariables MapType where
    extractVariables MapEmpty = []
    extractVariables (MapVar name) =
        [name]
    extractVariables (MapCons idTerm intTerm mapTerm) =
        extractVariables idTerm
        <> extractVariables intTerm
        <> extractVariables mapTerm

class SubstituteVariable term where
    substituteVariable :: term -> (Name, MiniK) -> term

instance SubstituteVariable RewriteRule where
    substituteVariable
        RewriteRule { left, right, sideCondition }
        subst
      =
        RewriteRule
            { left = substituteVariable left subst
            , right = substituteVariable right subst
            , sideCondition = substituteVariable sideCondition subst
            }

instance SubstituteVariable Konfiguration where
    substituteVariable
        Konfiguration { k, kState }
        subst
      =
        Konfiguration 
            { k = substituteVariable k subst
            , kState = substituteVariable kState subst
            }

instance SubstituteVariable MiniK where
    substituteVariable KEmpty _ = KEmpty
    substituteVariable (KInt intTerm) subst =
        KInt (substituteVariable intTerm subst)
    substituteVariable (KBool boolTerm) subst =
        KBool (substituteVariable boolTerm subst)
    substituteVariable (KMap mapTerm) subst =
        KMap (substituteVariable mapTerm subst)
    substituteVariable (KVar name) (varName, kTerm)
        | name == varName = kTerm
        | otherwise = KVar name
    substituteVariable (KSymbol symbolName args) subst =
        KSymbol
            symbolName
            (flip substituteVariable subst <$> args)
    substituteVariable (KSeq kTerm1 kTerm2) subst =
        KSeq
            (substituteVariable kTerm1 subst)
            (substituteVariable kTerm2 subst)

instance SubstituteVariable IntType where
    substituteVariable (I val) _ = I val
    substituteVariable (IntVar name) (varName, kTerm)
        | name == varName = 
            retractIntTerm kTerm
        | otherwise = IntVar name
    substituteVariable (IntId idTerm) subst =
        IntId (substituteVariable idTerm subst)
    substituteVariable (Plus intTerm1 intTerm2) subst =
        Plus
            (substituteVariable intTerm1 subst)
            (substituteVariable intTerm2 subst)
    substituteVariable (Mod intTerm1 intTerm2) subst =
        Mod
            (substituteVariable intTerm1 subst)
            (substituteVariable intTerm2 subst)

instance SubstituteVariable IdType where
    substituteVariable (Id name) _ = Id name
    substituteVariable (IdVar name) (varName, kTerm)
        | name == varName = 
            retractIdTerm kTerm
        | otherwise = IdVar name

instance SubstituteVariable BoolType where
    substituteVariable (B val) _ = B val
    substituteVariable (BoolVar name) (varName, kTerm)
        | name == varName = 
            retractBoolTerm kTerm
        | otherwise = BoolVar name
    substituteVariable (Not boolTerm) subst =
        Not (substituteVariable boolTerm subst)
    substituteVariable (And boolTerm1 boolTerm2) subst =
        And
            (substituteVariable boolTerm1 subst)
            (substituteVariable boolTerm2 subst)
    substituteVariable (LT' intTerm1 intTerm2) subst =
        LT'
            (substituteVariable intTerm1 subst)
            (substituteVariable intTerm2 subst)

instance SubstituteVariable MapType where
    substituteVariable MapEmpty _ = MapEmpty
    substituteVariable (MapVar name) (varName, kTerm)
        | name == varName =
            retractMapTerm kTerm
        | otherwise = MapVar name
    substituteVariable (MapCons idTerm intTerm mapTerm) subst =
        MapCons
            (substituteVariable idTerm subst)
            (substituteVariable intTerm subst)
            (substituteVariable mapTerm subst)
