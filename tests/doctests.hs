module Main where

import Build_doctests (deps)
import Test.DocTest
import Data.Maybe
import System.Environment

main = do
  env <- getEnvironment
  let dist = fromMaybe "dist" $ lookup "HASKELL_DIST_DIR" env
      autogenDir = dist ++ "/build/autogen"
      args = [ "-i."
             , "-i" ++ autogenDir
             , "-optP-include"
             , "-optP" ++ autogenDir ++ "/cabal_macros.h"
             , "-hide-all-packages"
             ] ++ map ("-package=" ++) deps
      sources = ["Web/Twitter/Conduit.hs"]
  doctest $ args ++ sources
