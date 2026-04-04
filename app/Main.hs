module Main (main) where

import Control.Monad (forever)
import Repl (run)

main :: IO ()
main = do
  forever run
