module Main where

import Rewrite
import MiniK
import qualified Languages.Imp as Imp

-- TODO: Add a program which contains some complexity.
-- For example, it can have some nested while loops,
-- each doing a high number of iterations.
--
-- This program will allow you to profile the execution
-- engine and measure performance.
inputProgram :: Konfiguration
inputProgram =
    Konfiguration 
        { k = KEmpty
        , kState = MapEmpty
        }

main :: IO ()
main =
    print $ rewrite inputProgram Imp.rewriteRules
