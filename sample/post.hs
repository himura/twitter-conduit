{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class
import Web.Twitter.Conduit
import System.Environment
import Common

main :: IO ()
main = runTwitterFromEnv' $ do
    status <- T.concat . map T.pack <$> liftIO getArgs
    liftIO $ T.putStrLn $ "Post message: " <> status
    res <- call $ update status
    liftIO $ print res
