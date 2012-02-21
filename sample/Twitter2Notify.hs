{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Web.Twitter.Enumerator

import Network.HTTP.Enumerator
import Data.Enumerator hiding (map)
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class
import Control.Applicative
import qualified Data.Text.Encoding as T
import Control.Monad
import System.IO
import System.Process (rawSystem)
import System.FilePath
import System.Directory
import System.Environment

main :: IO ()
main = withCF $ do
  (ln:_) <- liftIO getArgs
  curious <- fetchListAll ln
  let cid = map userId curious
  run_ $ userstream (EL.mapM (\x -> showTL x >> return x) =$ EL.filter (filterUsers cid) =$ iterNotify)
  where fetchListAll str = run_ $ listsMembers (QListName str) $$ EL.consume
        filterUsers ulist (SStatus s) = (\uid -> any (== uid) ulist) . userId . statusUser $ s
        filterUsers _ _ = False

iconPath :: IO FilePath
iconPath = (</> "icons") <$> confdir >>= ensureDirectoryExist

fetchIcon :: String -> String -> IO String
fetchIcon scName url = do
  ipath <- iconPath
  let fname = ipath </> scName ++ "__" ++ takeFileName url
  exists <- doesFileExist fname
  unless exists $ do
    h <- openBinaryFile fname WriteMode
    simpleHttp url >>= LB.hPut h
    hClose h
  return fname

notifySend :: String -> String -> Maybe FilePath -> IO ()
notifySend header content icon = do
  let ic = maybe [] (\i -> ["-i", i]) icon
  _ <- rawSystem "notify-send" $ [header, content] ++ ic
  return ()

iterNotify :: Iteratee StreamingAPI IO ()
iterNotify = do
  st <- EL.head
  case st of
    Just (SStatus s) -> do
      let user = statusUser s
          sn = userScreenName user
          cnt = B.unpack . T.encodeUtf8 $ statusText s
      icon <- case userProfileImageURL user of
        Nothing -> return Nothing
        Just url -> liftIO $ fetchIcon sn url >>= return . Just
      liftIO $ notifySend sn cnt icon
      iterNotify
    Just _ -> iterNotify
    Nothing -> return ()

showTL :: StreamingAPI -> IO ()
showTL (SStatus s) = do
  let user = statusUser s
      sn = B.pack $ userScreenName user
      cnt = T.encodeUtf8 $ statusText s
  B.putStrLn $ B.concat [sn, ": ", cnt]
showTL (SRetweetedStatus rs) =
  B.putStrLn $ B.concat ["Retweeted (by ", rtuser, "): ", user, ": ", T.encodeUtf8 text]
    where rtuser = B.pack . userScreenName . rsUser $ rs
          status = rsRetweetedStatus rs
          user = B.pack . userScreenName . statusUser $ status
          text = statusText status
showTL (SEvent ev) = do
  putStrLn $ "Event: " ++ evEvent ev
  putStr "> "
  showEventTarget $ evTarget ev
  putStr "> "
  showEventTarget $ evSource ev
  maybe (return ()) (\e -> putStr "> " >> showEventTarget e) $ evTargetObject ev
showTL _ = return ()

showEventTarget :: EventTarget -> IO ()
showEventTarget (ETUser u) =
  B.putStrLn $ B.concat ["@", screenName, " (followers:", fol, ", description:", des, ")"]
    where screenName = B.pack . userScreenName $ u
          des = maybe "" T.encodeUtf8 $ userDescription u
          fol = B.pack . maybe "" show $ userFollowers u
showEventTarget (ETStatus s) =
  B.putStrLn $ B.concat [user, ": ", T.encodeUtf8 text]
    where user = B.pack . userScreenName . statusUser $ s
          text = statusText s
showEventTarget (ETList l) =
  B.putStrLn $ B.concat ["List: ", fullname, ": ", memberCount]
    where fullname = B.pack . listFullName $ l
          memberCount = B.pack . show . listMemberCount $ l
showEventTarget o = putStrLn $ "unknown object: " ++ show o
