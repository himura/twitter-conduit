{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Example:
--   $ export OAUTH_CONSUMER_KEY="your consumer key"
--   $ export OAUTH_CONSUMER_SECRET="your consumer secret"
--   $ runhaskell oauth_pin.hs

module Main where

import Web.Twitter.Conduit hiding (lookup)
import Web.Authenticate.OAuth as OA
import qualified Data.ByteString.Char8 as S8
import Data.Maybe
import System.Environment
import System.IO (hFlush, stdout)

getTokens :: IO OAuth
getTokens = do
    consumerKey <- getEnv "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv "OAUTH_CONSUMER_SECRET"
    return $
        twitterOAuth
        { oauthConsumerKey = S8.pack consumerKey
        , oauthConsumerSecret = S8.pack consumerSecret
        , oauthCallback = Just "oob"
        }

authorize :: OAuth -- ^ OAuth Consumer key and secret
          -> Manager
          -> IO Credential
authorize oauth mgr = do
    cred <- OA.getTemporaryCredential oauth mgr
    let url = OA.authorizeUrl oauth cred
    pin <- getPIN url
    OA.getAccessToken oauth (OA.insert "oauth_verifier" pin cred) mgr
  where
    getPIN url = do
        putStrLn $ "browse URL: " ++ url
        putStr "> what was the PIN twitter provided you with? "
        hFlush stdout
        S8.getLine

main :: IO ()
main = do
    tokens <- getTokens
    mgr <- newManager tlsManagerSettings
    Credential cred <- authorize tokens mgr
    print cred

    S8.putStrLn . S8.intercalate "\n" $
        [ "export OAUTH_CONSUMER_KEY=\"" <> oauthConsumerKey tokens <> "\""
        , "export OAUTH_CONSUMER_SECRET=\"" <> oauthConsumerSecret tokens <> "\""
        , "export OAUTH_ACCESS_TOKEN=\"" <> fromMaybe "" (lookup "oauth_token" cred) <> "\""
        , "export OAUTH_ACCESS_SECRET=\"" <> fromMaybe "" (lookup "oauth_token_secret" cred) <> "\""
        ]
