module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  hints <- hlint $ ["Web", "--cpp-define=HLINT", "--cpp-ansi", "--cpp-file=dist/build/autogen/cabal_macros.h"] ++ args
  unless (null hints) exitFailure
