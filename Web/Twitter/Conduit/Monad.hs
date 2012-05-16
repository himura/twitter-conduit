{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Twitter.Conduit.Monad
       ( TW
       , TWEnv (..)
       , RequireAuth (..)
       , runTW
       , runTWManager
       , newEnv
       , getOAuth
       , getCredential
       , getProxy
       , getManager
       , signOAuthTW
       )
       where

import Web.Twitter.Conduit.Types
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Default
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import qualified Control.Exception.Lifted as E
import Control.Applicative

type TW = ReaderT TWEnv (ResourceT IO)

data TWEnv = TWEnv
             { twOAuth :: Maybe OAuth
             , twCredential :: Credential
             , twProxy :: Maybe Proxy
             , twManager :: Maybe Manager
             }

instance Default TWEnv where
  def = TWEnv
    { twOAuth = Nothing
    , twCredential = Credential []
    , twProxy = Nothing
    , twManager = Nothing
    }

data RequireAuth = NoAuth | AuthSupported | AuthRequired

runTW' :: TWEnv -> TW a -> IO a
runTW' env m = runResourceT $ runReaderT m env

runTW :: TWEnv -> TW a -> IO a
runTW env st =
  case twManager env of
    Nothing -> withManager $ \mgr -> liftIO $ runTWManager env mgr st
    Just _ -> runTW' env st

runTWManager :: TWEnv -> Manager -> TW a -> IO a
runTWManager env mgr st = runTW' env { twManager = Just mgr } st

{-# DEPRECATED newEnv "Use Data.Default.def instead" #-}
-- | DEPRECATED: newEnv Use 'Data.Default.def' instead
newEnv :: OAuth -> TWEnv
newEnv tokens
  = TWEnv
    { twOAuth = Just tokens
    , twCredential = Credential []
    , twProxy = Nothing
    , twManager = Nothing
    }

getOAuth :: TW OAuth
getOAuth = do
  oa' <- asks twOAuth
  case oa' of
    Just oa -> return oa
    Nothing -> E.throwIO MissingCredential

getCredential :: TW Credential
getCredential = asks twCredential

getProxy :: TW (Maybe Proxy)
getProxy = asks twProxy

getManager :: TW Manager
getManager = do
  mgr <- asks twManager
  case mgr of
    Just m -> return m
    Nothing -> error "getManager: manager is not initialized, should not be happen."

signOAuthTW :: RequireAuth -> Request TW -> TW (Request TW)
signOAuthTW authp req =
  case authp of
    NoAuth -> return req
    AuthSupported -> do
      credp <- haveCredential
      if credp
        then signOAuthTW' req
        else return req
    AuthRequired -> signOAuthTW' req

haveCredential :: TW Bool
haveCredential = do
  oa <- asks twOAuth
  case oa of
    Nothing -> return False
    Just _ -> not . null . unCredential <$> asks twCredential

signOAuthTW' :: Request TW -> TW (Request TW)
signOAuthTW' req = do
  oa' <- asks twOAuth
  cred <- asks twCredential
  case oa' of
    Nothing -> E.throwIO MissingCredential
    Just oa -> signOAuth oa cred req
