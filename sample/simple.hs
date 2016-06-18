{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Web.Twitter.Conduit
import qualified Web.Twitter.Conduit.Parameters as P
import Web.Twitter.Types.Lens

import Control.Lens
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)
import qualified Web.Authenticate.OAuth as OA

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = error "You MUST specify oauthConsumerKey parameter."
    , oauthConsumerSecret = error "You MUST specify oauthConsumerSecret parameter."
    }

authorize :: OAuth -- ^ OAuth Consumer key and secret
          -> (String -> IO String) -- ^ PIN prompt
          -> Manager
          -> IO Credential
authorize oauth getPIN mgr = do
    cred <- OA.getTemporaryCredential oauth mgr
    let url = OA.authorizeUrl oauth cred
    pin <- getPIN url
    OA.getAccessToken oauth (OA.insert "oauth_verifier" (B8.pack pin) cred) mgr

getTWInfo :: Manager -> IO TWInfo
getTWInfo mgr = do
    cred <- authorize tokens getPIN mgr
    return $ setCredential tokens cred def
  where
    getPIN url = do
        putStrLn $ "browse URL: " ++ url
        putStr "> what was the PIN twitter provided you with? "
        hFlush stdout
        getLine

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    twInfo <- getTWInfo mgr
    putStrLn $ "# your home timeline (up to 800 tweets):"
    sourceWithMaxId twInfo mgr (homeTimeline & P.count ?~ 200)
        C.$= CL.isolate 800
        C.$$ CL.mapM_ $ \status -> do
            T.putStrLn $ T.concat [ T.pack . show $ status ^. statusId
                                  , ": "
                                  , status ^. statusUser . userScreenName
                                  , ": "
                                  , status ^. statusText
                                  ]
