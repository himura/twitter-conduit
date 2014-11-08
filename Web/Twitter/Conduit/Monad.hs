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
       , twitterOAuth
       )
       where

import Web.Twitter.Conduit.Types
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

-- $setup
-- >>> :set -XOverloadedStrings

type TW m = ReaderT TWEnv m

data TWEnv = TWEnv
    { twInfo :: TWInfo
    , twManager :: Manager
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

signOAuthTW :: MonadIO m
            => Request
            -> TW m Request
signOAuthTW req = do
    TWToken oa cred <- asks (twToken . twInfo)
    signOAuth oa cred req
