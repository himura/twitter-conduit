{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Twitter.Conduit.Monad
       ( TW
       , NoToken
       , WithToken
       , AuthType (..)
       , TWInfo (..)
       , setCredential
       , runTW
       , runTWManager
       , getProxy
       , getManager
       , signOAuthTW
       , signOAuthIfExistTW
       )
       where

import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Default
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

type TW cred m = ReaderT (TWEnv cred) m

data NoToken
data WithToken

data AuthType a where
  NoAuth :: AuthType NoToken
  UseOAuth :: OAuth -> Credential -> AuthType WithToken

data TWInfo cred = TWInfo
    { twToken :: AuthType cred
    , twProxy :: Maybe Proxy
    }
instance Default (TWInfo NoToken) where
  def = TWInfo
        { twToken = NoAuth
        , twProxy = Nothing
        }

data TWEnv cred = TWEnv
    { twInfo :: TWInfo cred
    , twManager :: Manager
    }

setCredential :: OAuth -> Credential -> TWInfo NoToken -> TWInfo WithToken
setCredential oa cred env
  = TWInfo
    { twToken = UseOAuth oa cred
    , twProxy = twProxy env
    }

runTW :: ( MonadIO m
         , MonadBaseControl IO m
         , MonadThrow m
         , MonadUnsafeIO m )
      => TWInfo cred
      -> TW cred (ResourceT m) a
      -> m a
runTW info st = withManager $ \mgr -> runTWManager info mgr st

runTWManager :: MonadBaseControl IO m => TWInfo cred -> Manager -> TW cred (ResourceT m) a -> ResourceT m a
runTWManager info mgr st = runReaderT st $ TWEnv info mgr

getProxy ::Monad m => TW cred m (Maybe Proxy)
getProxy = asks (twProxy . twInfo)

getManager :: Monad m => TW cred m Manager
getManager = asks twManager

signOAuthTW :: (Monad m, MonadUnsafeIO m) => Request (TW WithToken m) -> TW WithToken m (Request (TW WithToken m))
signOAuthTW req = do
  UseOAuth oa cred <- asks (twToken . twInfo)
  signOAuth oa cred req

signOAuthIfExistTW :: (Monad m, MonadUnsafeIO m) => Request (TW cred m) -> TW cred m (Request (TW cred m))
signOAuthIfExistTW req = do
  token <- asks (twToken . twInfo)
  case token of
    UseOAuth oa cred -> signOAuth oa cred req
    NoAuth -> return req
