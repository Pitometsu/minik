module Rewrite
    ( rewrite
    , rewriteNormalize
    , rewriteStep
    ) where

import Control.Parallel.Strategies (parTraversable, rdeepseq, withStrategy)
import MiniK (Evaluated(..), Variability(..), OfKonfiguration (..), type KonfigurationConcr, type KonfigurationRedex, RewriteRule (..), OfRewrite (..), type RewriteVar, pattern I, type Konfiguration, normalizeK, rewriteOf, fromValueKonf, fromValueMap)
import Control.Monad qualified as Monad
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import CheckCondition (checkCondition, evaluate)
import Match (match)
import Substitute (substitute)
import NormalizedMap qualified (normalize, renormalizeTraverse)
-- import Debug.Trace

-- The rewriting procedure.
--
-- Rules are matched against the configuration, the resulting substitution
-- is used to instantiate the variables in the rule. The side condition
-- is checked, looking up identifiers in the program state.
--
-- Map renormalization is necesssary, to ensure a correctly built map
-- in the resulting configuration.
applyRewriteRule
    :: KonfigurationConcr
    -> RewriteRule
    -> Maybe Konfiguration
applyRewriteRule konfig rule = do
    matchResult <- match konfig $ left rule
    substituted <- substitute matchResult $ rewriteOf rule
    let requiredCondition = condition substituted
        state = NormalizedMap.normalize $ kState konfig
    Monad.guard (checkCondition state requiredCondition)
    kState'
        <- NormalizedMap.renormalizeTraverse @Redex @Value @Concrete
            (CheckCondition.evaluate state)
            . kState $ konf substituted
    pure $ ((\konf' -> konf' { k = normalizeK $ k konf' }) $ konf substituted)
        { kState = kState' }

-- Step once through the execution of the program.
-- Good for debugging.
rewriteStep
    :: KonfigurationConcr
    -> [RewriteRule]
    -> KonfigurationConcr
rewriteStep konfig = fromMaybe konfig . fmap fromValueKonf .rewriteStep' konfig

rewriteStep'
    :: KonfigurationConcr
    -> [RewriteRule]
    -> Maybe Konfiguration
rewriteStep' konfig =
    -- traceEvent "Rewrite step" $
    listToMaybe . parMapMaybe (applyRewriteRule konfig)
  where
    -- parMapMaybe f = withStrategy (parTraversable rdeepseq) . mapMaybe f
    parMapMaybe f = mapMaybe f

-- Fully execute the program by rewriting with the set of rewrite
-- rules.
rewrite'
    :: KonfigurationConcr
    -> [RewriteRule]
    -> Maybe Konfiguration
rewrite' konfig rewriteRules =
    -- traceEvent "Rewrite" $
    loop =<< rewriteKonfig konfig
  where
    rewriteKonfig = flip rewriteStep' rewriteRules
    loop konfig' = maybe (pure konfig')
        loop . rewriteKonfig $ fromValueKonf konfig'

rewrite
    :: KonfigurationConcr
    -> [RewriteRule]
    -> KonfigurationConcr
rewrite konfig = fromMaybe konfig
    . fmap fromValueKonf . rewrite' konfig

rewriteNormalize
    :: KonfigurationRedex
    -> [RewriteRule]
    -> KonfigurationConcr
rewriteNormalize konfig = rewrite konfig { k = normalizeK $ k konfig }
