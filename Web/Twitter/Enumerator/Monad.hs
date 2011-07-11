module Web.Twitter.Enumerator.Monad
       ( TW
       , TWEnv (..)
       , runTW
       , newEnv
       , getOAuth
       , putOAuth
       , getCredential
       , putCredential
       , getProxy
       , putProxy
       )
       where

import Web.Authenticate.OAuth
import Network.HTTP.Enumerator
import Control.Monad.State

type TW = StateT TWEnv IO

data TWEnv = TWEnv
             { twOAuth :: OAuth
             , twCredential :: Credential
             , twProxy :: Maybe Proxy
             }

runTW :: TWEnv -> TW a -> IO a
runTW env st = fst `fmap` runStateT st env

newEnv :: OAuth -> TWEnv
newEnv tokens
  = TWEnv
    { twOAuth = tokens
    , twCredential = Credential []
    , twProxy = Nothing
    }

getOAuth :: TW OAuth
getOAuth = twOAuth `fmap` get

putOAuth :: OAuth -> TW ()
putOAuth oauth = do
  env <- get
  put env { twOAuth = oauth}

getCredential :: TW Credential
getCredential = twCredential `fmap` get

putCredential :: Credential -> TW ()
putCredential cred = do
  env <- get
  put env { twCredential = cred }

getProxy :: TW (Maybe Proxy)
getProxy = twProxy `fmap` get

putProxy :: Maybe Proxy -> TW ()
putProxy p = do
  env <- get
  put env { twProxy = p }
