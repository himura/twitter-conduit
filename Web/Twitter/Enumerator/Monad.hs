module Web.Twitter.Enumerator.Monad
       ( TW
       , TWEnv (..)
       , runTW
       , newEnv
       , getOAuth
       , getCredential
       , getProxy
       , signOAuthTW
       )
       where

import Web.Authenticate.OAuth
import Network.HTTP.Enumerator
import Control.Monad.Reader
import Control.Applicative

type TW = ReaderT TWEnv IO

data TWEnv = TWEnv
             { twOAuth :: OAuth
             , twCredential :: Credential
             , twProxy :: Maybe Proxy
             , twManager :: Manager
             }

runTW :: TWEnv -> TW a -> IO a
runTW env st = runReaderT st env

newEnv :: OAuth -> Manager -> TWEnv
newEnv tokens manager
  = TWEnv
    { twOAuth = tokens
    , twCredential = Credential []
    , twProxy = Nothing
    , twManager = manager
    }

getOAuth :: TW OAuth
getOAuth = twOAuth `fmap` ask

getCredential :: TW Credential
getCredential = twCredential `fmap` ask

getProxy :: TW (Maybe Proxy)
getProxy = twProxy `fmap` ask

signOAuthTW :: Request IO -> TW (Request IO)
signOAuthTW req = do
  oa <- getOAuth
  cred <- getCredential
  liftIO $ signOAuth oa cred req
