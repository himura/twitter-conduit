module Main where

import Build_doctests (flags, module_sources, pkgs)
import Data.Foldable (traverse_)
import Test.DocTest (doctest)

main :: IO ()
main = do
    traverse_ putStrLn args -- optionally print arguments
    doctest args
  where
    args = flags ++ pkgs ++ module_sources
