{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans
import System.Environment

import Web.Twitter.Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default

main :: IO ()
main = runTW (def :: TWInfo NoToken) $ do
  [keyword] <- liftIO getArgs

  res <- searchSource keyword []
  liftIO . putStrLn $ "search completed in: " ++ (show . searchResultCompletedIn $ res)
  liftIO . putStrLn $ "search result max id: " ++ (show . searchResultMaxId $ res)
  searchResultResults res $$ CL.mapM_ (liftIO . print)
