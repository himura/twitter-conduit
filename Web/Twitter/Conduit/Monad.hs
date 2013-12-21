{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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
       )
       where

import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Default
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

type TW m = ReaderT TWEnv m

data TWToken
    = TWToken
      { twOAuth :: OAuth
      , twCredential :: Credential
      }
instance Default TWToken where
  def = TWToken defaultTokens (Credential [])

data TWInfo = TWInfo
    { twToken :: TWToken
    , twProxy :: Maybe Proxy
    }
instance Default TWInfo where
  def = TWInfo
        { twToken = def
        , twProxy = Nothing
        }

defaultTokens :: OAuth
defaultTokens =
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

setCredential :: OAuth -> Credential -> TWInfo -> TWInfo
setCredential oa cred env
  = TWInfo
    { twToken = TWToken oa cred
    , twProxy = twProxy env
    }

runTW :: ( MonadIO m
         , MonadBaseControl IO m
         , MonadThrow m
         , MonadUnsafeIO m )
      => TWInfo
      -> TW (ResourceT m) a
      -> m a
runTW info st = withManager $ \mgr -> runTWManager info mgr st

runTWManager :: MonadBaseControl IO m => TWInfo -> Manager -> TW (ResourceT m) a -> ResourceT m a
runTWManager info mgr st = runReaderT st $ TWEnv info mgr

getProxy ::Monad m => TW m (Maybe Proxy)
getProxy = asks (twProxy . twInfo)

getManager :: Monad m => TW m Manager
getManager = asks twManager

signOAuthTW :: (Monad m, MonadUnsafeIO m)
#if MIN_VERSION_http_conduit(2, 0, 0)
            => Request
            -> TW m Request
#else
            => Request (TW m)
            -> TW m (Request (TW m))
#endif
signOAuthTW req = do
  TWToken oa cred <- asks (twToken . twInfo)
  signOAuth oa cred req
