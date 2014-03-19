{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Example:
--   $ export OAUTH_CONSUMER_KEY="your consumer key"
--   $ export OAUTH_CONSUMER_SECRET="your consumer secret"
--   $ runhaskell oauth_pin.hs

module Main where

import Web.Twitter.Conduit
import Web.Authenticate.OAuth as OA
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import qualified Data.ByteString.Char8 as S8
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
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

authorize :: (MonadBaseControl IO m, C.MonadResource m)
          => OAuth -- ^ OAuth Consumer key and secret
          -> Manager
          -> m Credential
authorize oauth mgr = do
    cred <- OA.getTemporaryCredential oauth mgr
    let url = OA.authorizeUrl oauth cred
    pin <- getPIN url
    OA.getAccessToken oauth (OA.insert "oauth_verifier" pin cred) mgr
  where
    getPIN url = liftIO $ do
        putStrLn $ "browse URL: " ++ url
        putStr "> what was the PIN twitter provided you with? "
        hFlush stdout
        S8.getLine

main :: IO ()
main = do
    tokens <- getTokens
    cred <- liftIO $ withManager $ authorize tokens
    print cred
