module Main where

import Test.DocTest
import System.Directory
import System.FilePath
import Data.List
import Control.Monad
import Control.Applicative

main = do
  -- sources <- findSources "Web"
  doctest $ args -- ++ sources
  where
    args = [ "-i."
           , "-idist/build/autogen"
           , "-optP-include"
           , "-optPdist/build/autogen/cabal_macros.h"
           , "Web/Twitter/Conduit.hs"
           ]
