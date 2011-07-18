{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Twitter.Enumerator

import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA
import qualified Network.URI as URI
import Network.HTTP.Enumerator
import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
import Data.Enumerator hiding (map)
import qualified Data.Enumerator.List as EL
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec
import Control.Monad.IO.Class
import Control.Applicative
import qualified Data.Text.Encoding as T
import Control.Monad
import System.IO
import System.Process (rawSystem)
import System.FilePath
import System.Directory
import System.Environment

import Data.List
import Data.Char

tokens :: OAuth
tokens = OAuth { oauthServerName = "twitter"
               , oauthRequestUri = "http://twitter.com/oauth/request_token"
               , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
               , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
               , oauthConsumerKey = "<consumer key>"
               , oauthConsumerSecret = "<consumer secret>"
               , oauthSignatureMethod = OA.HMACSHA1
               , oauthCallback = Nothing
               }

main :: IO ()
main = withCF $ do
  curious <- fetchListAll "yourid/curious"
  let cid = map userId curious
  run_ $ userstream (EL.mapM (\x -> showTL x >> return x) =$ EL.filter (filterUsers cid) =$ iterNotify)
  where withCF t = credentialFile >>= \f -> withCredentialFile f t
        fetchListAll str = run_ $ listsMembers (Left str) $$ EL.consume
        filterUsers ulist (SStatus s) = (\uid -> any (== uid) ulist) . userId . statusUser $ s
        filterUsers _ _ = False

ensureDirectoryExist :: FilePath -> IO FilePath
ensureDirectoryExist dir = do
  exist <- doesDirectoryExist dir
  when (not exist) $ createDirectory dir
  return dir

confdir :: IO FilePath
confdir = fmap (</> ".twitter2notify") getHomeDirectory >>= ensureDirectoryExist

credentialFile :: IO FilePath
credentialFile = (</> "credential.json") <$> confdir

iconPath :: IO FilePath
iconPath = (</> "icons") <$> confdir >>= ensureDirectoryExist

getProxyEnv :: IO (Maybe Proxy)
getProxyEnv = do
  env <- M.fromList <$> map (\(k,v) -> (map toLower k, v)) <$> getEnvironment
  let u = M.lookup "https_proxy" env <|>
          M.lookup "http_proxy" env <|>
          M.lookup "proxy" env >>= URI.parseURI >>= URI.uriAuthority
  return $ Proxy <$> (B.pack . URI.uriRegName <$> u) <*> (parsePort . URI.uriPort <$> u)
  where
    parsePort :: String -> Int
    parsePort []       = 8080
    parsePort (':':xs) = read xs
    parsePort xs       = error $ "port number parse failed " ++ xs

loadCredential :: FilePath -> IO (Maybe Credential)
loadCredential file = do
  existp <- doesFileExist file
  if existp
    then
    do
      content <- B.readFile file
      return $ (maybeResult . parse json) content >>= parseMaybe parseJSON >>= return . Credential
    else return Nothing

saveCredential :: FilePath -> Credential -> IO ()
saveCredential file cred = do
  LB.writeFile file $ encode . unCredential $ cred

withCredentialFile :: FilePath -> TW a -> IO a
withCredentialFile file task = do
  pr <- getProxyEnv
  cred <- maybe (authorize pr tokens getPIN) return =<< loadCredential file
  saveCredential file cred
  let env = newEnv tokens
  ret <- runTW env { twCredential = cred, twProxy = pr } $ task
  return $ ret
  where
    getPIN url = do
      putStrLn $ "browse URL: " ++ url
      putStr "> what was the PIN twitter provided you with? "
      hFlush stdout
      getLine

authorize :: Maybe Proxy -> OAuth -> (String -> IO String) -> IO Credential
authorize pr oauth getPIN = do
  cred <- OA.getTemporaryCredentialProxy pr oauth
  let url = OA.authorizeUrl oauth cred
  pin <- getPIN url
  OA.getAccessTokenProxy pr oauth $ OA.insert "oauth_verifier" (B.pack pin) cred

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
  rawSystem "notify-send" $ [header, content] ++ ic
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
  B.putStrLn $ B.concat ["@", screenName, " (followers:", fol, ", description:", T.encodeUtf8 . userDescription $ u, ")"]
    where screenName = B.pack . userScreenName $ u
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
