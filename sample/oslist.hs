{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Web.Twitter.Enumerator

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import System.Environment
import System.IO

main :: IO ()
main = withCF $ C.runResourceT $ do
  src <- userstream
  transResourceT lift $ src C.$$ CL.mapM_ print

{-
main :: IO ()
main = withCF $ do
  (screenName:ln:_) <- liftIO getArgs
  let sn = QScreenName screenName
  onesideList <- consumeRun $ listsMembers $ QListName ln
  folids <- consumeRun $ followersIds sn
  friids <- consumeRun $ friendsIds sn
  let oslstmap = M.fromList $ map (flip (,) True . userId) onesideList
      folmap = M.fromList $ map (flip (,) True) folids
      os = filter (\uid -> M.notMember uid folmap && M.notMember uid oslstmap) friids
      bothfollow = filter (\usr -> M.member (userId usr) folmap) onesideList

  liftIO . putStrLn $ "one sided:"
  forM_ os $ \uid -> do
    usr <- usersShow . QUserId $ uid
    liftIO . showUser $ usr

  liftIO . putStrLn $ "both following:"
  forM_ bothfollow $ liftIO . showUser
  where consumeRun f = C.runResourceT $ f C.$$ CL.consume
        showUser usr = do
          putStr . show . userId $ usr
          putStr ":"
          putStrLn . userScreenName $ usr
-}
