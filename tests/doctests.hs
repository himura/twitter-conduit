module Main where

import Test.DocTest
import System.Directory
import System.FilePath
import Data.List
import Control.Monad
import Control.Applicative

main = do
  sources <- findSources "Web"
  doctest $ args ++ sources
  where
    args = [ "-i."
           , "-idist/build/autogen"
           , "-optP-include"
           , "-optPdist/build/autogen/cabal_macros.h"
           ]

findSources :: FilePath -> IO [FilePath]
findSources dir = filter (isSuffixOf ".hs") <$> go dir
  where
    go dir = do
      (dirs, files) <- listFiles dir
      (files ++) . concat <$> mapM go dirs
    listFiles dir = do
      entries <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
      (,) <$> filterM doesDirectoryExist entries <*> filterM doesFileExist entries
