{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import System.Environment

import Web.Twitter.Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default
import Common

main :: IO ()
main = withCF $ do
  [keyword] <- liftIO getArgs

  res <- searchSource keyword []
  let metadata = searchResultSearchMetadata res
  liftIO . putStrLn $ "search completed in: " ++ (show . searchMetadataCompletedIn $ metadata)
  liftIO . putStrLn $ "search result max id: " ++ (show . searchMetadataMaxId $ metadata)
  searchResultStatuses res $$ CL.mapM_ (liftIO . print)
