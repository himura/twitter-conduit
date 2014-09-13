{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Control.Lens
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B8
import Data.Default
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import System.IO (hFlush, stdout)

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = error "You MUST specify oauthConsumerKey parameter."
    , oauthConsumerSecret = error "You MUST specify oauthConsumerSecret parameter."
    }

authorize :: (MonadBaseControl IO m, MonadResource m)
          => OAuth -- ^ OAuth Consumer key and secret
          -> (String -> m String) -- ^ PIN prompt
          -> Manager
          -> m Credential
authorize oauth getPIN mgr = do
    cred <- OA.getTemporaryCredential oauth mgr
    let url = OA.authorizeUrl oauth cred
    pin <- getPIN url
    OA.getAccessToken oauth (OA.insert "oauth_verifier" (B8.pack pin) cred) mgr

withCredential :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) => TW (ResourceT m) a -> m a
withCredential task = do
    cred <- liftIO $ withManager $ \mgr -> authorize tokens getPIN mgr
    let env = setCredential tokens cred def
    runTW env task
  where
    getPIN url = liftIO $ do
        putStrLn $ "browse URL: " ++ url
        putStr "> what was the PIN twitter provided you with? "
        hFlush stdout
        getLine

main :: IO ()
main = runNoLoggingT . withCredential $ do
    liftIO . putStrLn $ "# your home timeline (up to 100 tweets):"
    sourceWithMaxId homeTimeline
        C.$= CL.isolate 100
        C.$$ CL.mapM_ $ \status -> liftIO $ do
            T.putStrLn $ T.concat [ T.pack . show $ status ^. statusId
                                  , ": "
                                  , status ^. statusUser . userScreenName
                                  , ": "
                                  , status ^. statusText
                                  ]
