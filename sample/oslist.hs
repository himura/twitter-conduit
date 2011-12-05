module Main where

import Common
import Web.Twitter.Enumerator

import Data.Enumerator hiding (map, filter)
import qualified Data.Enumerator.List as EL
import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad
import System.Environment

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
  where consumeRun f = run_ $ f $$ EL.consume
        showUser usr = do
          putStr . show . userId $ usr
          putStr ":"
          putStrLn . userScreenName $ usr
