module Main where

import Build_doctests (deps)
import Test.DocTest
import System.Directory
import System.FilePath
import Data.List
import Control.Monad
import Control.Applicative

main = do
  let sources =
          [ "Web/Twitter/Conduit.hs"
          ]

  doctest $ args ++ sources
  where
    args = ( "-i."
           : "-idist/build/autogen"
           : "-optP-include"
           : "-optPdist/build/autogen/cabal_macros.h"
           : "-hide-all-packages"
           : map ("-package=" ++) deps
           )

