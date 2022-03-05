module Rewrite
    ( rewrite
    , rewriteStep
    ) where

import MiniK (Konfiguration (..), RewriteRule (..), pattern I, reNormalizeK)
import qualified Control.Monad as Monad
import Data.Maybe (mapMaybe)
import CheckCondition (checkCondition, evaluate)
import Match (match)
import Substitute (substitute)
import qualified NormalizedMap
import Debug.Trace

-- The rewriting procedure.
--
-- Rules are matched against the configuration, the resulting substitution
-- is used to instantiate the variables in the rule. The side condition
-- is checked, looking up identifiers in the program state.
--
-- Map renormalization is necesssary, to ensure a correctly built map
-- in the resulting configuration.
applyRewriteRule
    :: Konfiguration
    -> RewriteRule
    -> Maybe Konfiguration
applyRewriteRule konfig rule = do
    matchResult <- match konfig $ left rule
    substitutedRule <- substitute matchResult rule
    let requiredCondition = sideCondition substitutedRule
        state =
            NormalizedMap.normalize
            . kState
            . left
            $ substitutedRule
    Monad.guard (checkCondition state requiredCondition)
    let konfigState = NormalizedMap.normalize $ kState konfig
        processedNewKonfig =
            ((\konf -> konf {k = reNormalizeK $ k konf}) $ right substitutedRule)
                { kState = NormalizedMap.unNormalize
                      . NormalizedMap.map (I . evaluate konfigState)
                      . NormalizedMap.normalize
                      . kState $ right substitutedRule
                }
    return processedNewKonfig

-- Step once through the execution of the program.
-- Good for debugging.
rewriteStep
    :: Konfiguration
    -> [RewriteRule]
    -> Konfiguration
rewriteStep konfig = rewriteStep ((\konf -> konf {k = reNormalizeK $ k konf}) konfig)

rewriteStep'
    :: Konfiguration
    -> [RewriteRule]
    -> Konfiguration
rewriteStep' konfig rewriteRules =
    traceEvent "Rewrite step" $
    let results =
            mapMaybe (applyRewriteRule konfig) rewriteRules
     in
        case results of
            [] -> konfig
            (konfig' : _) -> konfig'

-- Fully execute the program by rewriting with the set of rewrite
-- rules.
rewrite
    :: Konfiguration
    -> [RewriteRule]
    -> Konfiguration
rewrite konfig rewriteRules =
    traceEvent "Rewrite" $
    loop ((\konf -> konf {k = reNormalizeK $ k konf}) konfig)
  where
    loop input =
        let output = rewriteStep' input rewriteRules
         in
            if input == output
                then input
                else loop output
