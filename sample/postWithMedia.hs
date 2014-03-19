{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Control.Monad.IO.Class
import Web.Twitter.Conduit
import System.Environment
import Common

main :: IO ()
main = runTwitterFromEnv' $ do
    [status, filepath] <- liftIO getArgs
    liftIO $ putStrLn $ "Post message: " ++ status
    res <- call $ updateWithMedia (T.pack status) (MediaFromFile filepath)
    liftIO $ print res
