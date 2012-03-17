{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Twitter.Enumerator
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA
import Data.Enumerator (($$), ($=), run_)
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B8
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)

tokens :: OAuth
tokens = OAuth { oauthServerName = "twitter"
               , oauthRequestUri = "http://twitter.com/oauth/request_token"
               , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
               , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
               , oauthConsumerKey = "PtIqT5VLGKZipeKnqeJMQ"
               , oauthConsumerSecret = "2lmI32rnFsRJuv5NCQ35UP5zWg58z986DliUsNeA"
               , oauthSignatureMethod = OA.HMACSHA1
               , oauthCallback = Nothing
               }

authorize :: OAuth -- ^ OAuth Consumer key and secret
          -> (String -> IO String) -- ^ PIN prompt
          -> IO Credential
authorize oauth getPIN = do
  cred <- OA.getTemporaryCredential oauth
  let url = OA.authorizeUrl oauth cred
  pin <- getPIN url
  OA.getAccessToken oauth $ OA.insert "oauth_verifier" (B8.pack pin) cred

withCredential :: TW a -> IO a
withCredential task = do
  cred <- authorize tokens getPIN
  let env = newEnv tokens
  runTW env { twCredential = cred } task
  where
    getPIN url = do
      putStrLn $ "browse URL: " ++ url
      putStr "> what was the PIN twitter provided you with? "
      hFlush stdout
      getLine

main :: IO ()
main = withCredential $ do
  liftIO . putStrLn $ "# your home timeline (up to 100 tweets):"
  run_
    $ statusesHomeTimeline []
    $= EL.isolate 100
    $$ EL.mapM_ $ \status -> liftIO $ do
      let sn = T.pack . userScreenName . statusUser $ status
          tweet = statusText status
      T.putStrLn $ T.concat [ sn, ": ", tweet]
