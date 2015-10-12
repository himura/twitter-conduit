module Main where

import Control.Monad
import Data.Maybe
import Language.Haskell.HLint
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  cabalMacros <- getCabalMacrosPath
  hints <- hlint $ ["Web", "--cpp-define=HLINT", "--cpp-ansi", "--cpp-file=" ++ cabalMacros] ++ args
  unless (null hints) exitFailure

getCabalMacrosPath :: IO FilePath
getCabalMacrosPath = do
    env <- getEnvironment
    let dist = fromMaybe "dist" $ lookup "HASKELL_DIST_DIR" env
    return $ dist ++ "/build/autogen/cabal_macros.h"
