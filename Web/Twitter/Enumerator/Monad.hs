module Web.Twitter.Enumerator.Monad
       ( TW
       , TWEnv (..)
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

import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Control.Monad.Trans
import Control.Monad.Reader

type TW = ReaderT TWEnv IO

data TWEnv = TWEnv
             { twOAuth :: OAuth
             , twCredential :: Credential
             , twProxy :: Maybe Proxy
             , twManager :: Maybe Manager
             }

runTW' :: TWEnv -> TW a -> IO a
runTW' = flip runReaderT

runTW :: TWEnv -> TW a -> IO a
runTW env st =
  case twManager env of
    Nothing -> withManager $ \mgr -> liftIO $ runTWManager env mgr st
    Just _ -> runTW' env st

runTWManager :: TWEnv -> Manager -> TW a -> IO a
runTWManager env mgr st = runTW' env { twManager = Just mgr } st

newEnv :: OAuth -> TWEnv
newEnv tokens
  = TWEnv
    { twOAuth = tokens
    , twCredential = Credential []
    , twProxy = Nothing
    , twManager = Nothing
    }

getOAuth :: TW OAuth
getOAuth = asks twOAuth

getCredential :: TW Credential
getCredential = asks twCredential

getProxy :: TW (Maybe Proxy)
getProxy = asks twProxy

getManager :: TW Manager
getManager = do
  mgr <- asks twManager
  case mgr of
    Just m -> return m
    Nothing -> error "manager is not initialized"

signOAuthTW :: Request IO -> TW (Request IO)
signOAuthTW req = do
  oa <- getOAuth
  cred <- getCredential
  liftIO $ signOAuth oa cred req
