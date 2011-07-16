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
import Network.HTTP.Enumerator
import Control.Monad.Reader
import Control.Applicative
import Data.Maybe

type TW = ReaderT TWEnv IO

data TWEnv = TWEnv
             { twOAuth :: OAuth
             , twCredential :: Credential
             , twProxy :: Maybe Proxy
             , twManager :: Maybe Manager
             }

runTW' :: TWEnv -> TW a -> IO a
runTW' env st = runReaderT st env

runTW :: TWEnv -> TW a -> IO a
runTW env st =
  case twManager env of
    Nothing -> withManager $ \mgr -> runTWManager env mgr st
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
getOAuth = twOAuth `fmap` ask

getCredential :: TW Credential
getCredential = twCredential `fmap` ask

getProxy :: TW (Maybe Proxy)
getProxy = twProxy `fmap` ask

getManager :: TW Manager
getManager = do
  mgr <- twManager `fmap` ask
  case mgr of
    Just m -> return m
    Nothing -> error "manager is not initialized"

signOAuthTW :: Request IO -> TW (Request IO)
signOAuthTW req = do
  oa <- getOAuth
  cred <- getCredential
  liftIO $ signOAuth oa cred req
