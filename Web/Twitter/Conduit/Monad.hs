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
       , TWEnv (..)
       , setCredential
       , runTW
       , runTWManager
       , getOAuth
       , getCredential
       , getProxy
       , getManager
       , signOAuthTW
       , signOAuthIfExistTW
       )
       where

import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Default
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Reader

type TW cred m = ReaderT (TWEnv cred) m

data NoToken
data WithToken

data AuthType a where
  NoAuth :: AuthType NoToken
  UseOAuth :: OAuth -> Credential -> AuthType WithToken

data TWEnv cred
  = TWEnv
    { twToken :: AuthType cred
    , twProxy :: Maybe Proxy
    , twManager :: Maybe Manager
    }

instance Default (TWEnv NoToken) where
  def = TWEnv
        { twToken = NoAuth
        , twProxy = Nothing
        , twManager = Nothing
        }

setCredential :: OAuth -> Credential -> TWEnv NoToken -> TWEnv WithToken
setCredential oa cred env
  = TWEnv
    { twToken = UseOAuth oa cred
    , twProxy = twProxy env
    , twManager = twManager env
    }

runTW' :: MonadBaseControl IO m => TWEnv cred -> TW cred m a -> m a
runTW' env m = runReaderT m env

runTW :: ( MonadIO m
         , MonadBaseControl IO m
         , MonadThrow m
         , MonadUnsafeIO m )
      => TWEnv cred
      -> TW cred (ResourceT m) a
      -> m a
runTW env st = withManager $ \mgr -> runTWManager env mgr st

runTWManager :: MonadBaseControl IO m => TWEnv cred -> Manager -> TW cred (ResourceT m) a -> ResourceT m a
runTWManager env mgr st = runTW' env { twManager = Just mgr } st

{-# DEPRECATED getOAuth "Use 'asks twToken' instead" #-}
getOAuth :: Monad m => TW WithToken m OAuth
getOAuth = do
  UseOAuth oa _ <- asks twToken
  return oa

{-# DEPRECATED getCredential "Use 'asks twToken' instead" #-}
getCredential :: Monad m => TW WithToken m Credential
getCredential = do
  UseOAuth _ cred <- asks twToken
  return cred

getProxy ::Monad m => TW cred m (Maybe Proxy)
getProxy = asks twProxy

getManager :: Monad m => TW cred m Manager
getManager = do
  mgr <- asks twManager
  case mgr of
    Just m -> return m
    Nothing -> error "getManager: manager is not initialized, should not be happen."

signOAuthTW :: (Monad m, MonadUnsafeIO m) => Request (TW WithToken m) -> TW WithToken m (Request (TW WithToken m))
signOAuthTW req = do
  UseOAuth oa cred <- asks twToken
  signOAuth oa cred req

signOAuthIfExistTW :: (Monad m, MonadUnsafeIO m) => Request (TW cred m) -> TW cred m (Request (TW cred m))
signOAuthIfExistTW req = do
  token <- asks twToken
  case token of
    UseOAuth oa cred -> signOAuth oa cred req
    NoAuth -> return req
