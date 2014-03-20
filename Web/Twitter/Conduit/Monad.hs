{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Web.Twitter.Conduit.Monad
       ( TW
       , TWToken (..)
       , TWInfo (..)
       , setCredential
       , runTW
       , runTWManager
       , getProxy
       , getManager
       , signOAuthTW
       , twitterOAuth
       )
       where

import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Default
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

-- $setup
-- >>> :set -XOverloadedStrings

type TW m = ReaderT TWEnv m

data TWToken = TWToken
    { twOAuth :: OAuth
    , twCredential :: Credential
    } deriving Show
instance Default TWToken where
    def = TWToken twitterOAuth (Credential [])

data TWInfo = TWInfo
    { twToken :: TWToken
    , twProxy :: Maybe Proxy
    } deriving Show
instance Default TWInfo where
    def = TWInfo
        { twToken = def
        , twProxy = Nothing
        }

twitterOAuth :: OAuth
twitterOAuth =
    def { oauthServerName = "twitter"
        , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
        , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
        , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
        , oauthConsumerKey = error "You MUST specify oauthConsumerKey parameter."
        , oauthConsumerSecret = error "You MUST specify oauthConsumerSecret parameter."
        , oauthSignatureMethod = HMACSHA1
        , oauthCallback = Nothing
        }

data TWEnv = TWEnv
    { twInfo :: TWInfo
    , twManager :: Manager
    }

-- | set OAuth keys and Credentials to TWInfo.
--
-- >>> let proxy = Proxy "localhost" 8080
-- >>> let twinfo = def { twProxy = Just proxy }
-- >>> let oauth = twitterOAuth { oauthConsumerKey = "consumer_key", oauthConsumerSecret = "consumer_secret" }
-- >>> let credential = Credential [("oauth_token","...")]
-- >>> let twinfo2 = setCredential oauth credential twinfo
-- >>> oauthConsumerKey . twOAuth . twToken $ twinfo2
-- "consumer_key"
-- >>> twProxy twinfo2 == Just proxy
-- True
setCredential :: OAuth -> Credential -> TWInfo -> TWInfo
setCredential oa cred env
  = TWInfo
    { twToken = TWToken oa cred
    , twProxy = twProxy env
    }

-- | create a new http-conduit manager and run TW monad.
--
-- >>> runTW def getProxy
-- Nothing
-- >>> runTW def $ asks (twCredential . twToken . twInfo)
-- Credential {unCredential = []}
runTW :: ( MonadBaseControl IO m
         , MonadIO m
         ) => TWInfo -> TW (ResourceT m) a -> m a
runTW info st = withManager $ \mgr -> runTWManager info mgr st

runTWManager :: MonadBaseControl IO m => TWInfo -> Manager -> TW (ResourceT m) a -> ResourceT m a
runTWManager info mgr st = runReaderT st $ TWEnv info mgr

getProxy ::Monad m => TW m (Maybe Proxy)
getProxy = asks (twProxy . twInfo)

getManager :: Monad m => TW m Manager
getManager = asks twManager

signOAuthTW :: MonadUnsafeIO m
            => Request
            -> TW m Request
signOAuthTW req = do
    TWToken oa cred <- asks (twToken . twInfo)
    signOAuth oa cred req
