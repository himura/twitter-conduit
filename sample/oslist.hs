{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Web.Twitter

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import System.Environment
import System.IO

main :: IO ()
main = withCF $ do
  (screenName:_) <- liftIO getArgs
  let sn = QScreenName screenName
  folids <- runResourceT $ do
    src <- followersIds sn
    transResourceT lift $ src C.$$ CL.consume
  friids <- runResourceT $ do
    src <- friendsIds sn
    transResourceT lift $ src C.$$ CL.consume

  let folmap = M.fromList $ map (flip (,) True) folids
      os = filter (\uid -> M.notMember uid folmap) friids
      bo = filter (\usr -> M.member usr folmap) friids

  liftIO $ putStrLn "one sided:"
  liftIO $ print os

  liftIO $ putStrLn "both following:"
  liftIO $ print bo
