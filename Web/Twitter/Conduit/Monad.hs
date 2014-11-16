{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Web.Twitter.Conduit.Monad
       (
       -- * How to use this library
       -- $howto

       -- * Re-exports
         module Web.Twitter.Conduit.Api
       , module Web.Twitter.Conduit.Cursor
       , module Web.Twitter.Conduit.Parameters
       , module Web.Twitter.Conduit.Request
       , module Web.Twitter.Conduit.Response
       , module Web.Twitter.Conduit.Status
       , module Web.Twitter.Conduit.Stream
       , module Web.Twitter.Conduit.Types

       -- * 'Web.Twitter.Conduit.Monad'
       , Base.TwitterBaseM
       , TW
       , runTW
       , runTWManager
       , call
       , call'
       , callWithResponse
       , callWithResponse'
       , sourceWithMaxId
       , sourceWithMaxId'
       , sourceWithCursor
       , sourceWithCursor'
       ) where

import Web.Twitter.Conduit.Api
import qualified Web.Twitter.Conduit.Base as Base
import Web.Twitter.Conduit.Cursor
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Response
import Web.Twitter.Conduit.Status
import Web.Twitter.Conduit.Stream
import Web.Twitter.Conduit.Types
import qualified Network.HTTP.Conduit as HTTP
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Aeson
import Web.Twitter.Types.Lens
import qualified Data.Conduit as C
import Control.Monad.Trans.Class (lift)

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif

-- $setup
-- >>> :set -XOverloadedStrings

type TW m = ReaderT TWEnv m

data TWEnv = TWEnv TWInfo HTTP.Manager

-- | create a new http-conduit manager and run TW monad.
--
-- >>> runTW def getProxy
-- Nothing
-- >>> runTW def $ asks (twCredential . twToken . twInfo)
-- Credential {unCredential = []}
runTW :: ( MonadBaseControl IO m
         , MonadIO m
         ) => TWInfo -> TW (ResourceT m) a -> m a
runTW info st = HTTP.withManager $ \mgr -> runTWManager info mgr st

runTWManager :: MonadBaseControl IO m => TWInfo -> HTTP.Manager -> TW (ResourceT m) a -> ResourceT m a
runTWManager info mgr st = runReaderT st $ TWEnv info mgr

call :: (MonadResource m, FromJSON responseType)
     => APIRequest apiName responseType
     -> TW m responseType
call = call'

call' :: (MonadResource m, FromJSON value)
      => APIRequest apiName responseType
      -> TW m value
call' req = do
    TWEnv info mgr <- ask
    lift $ Base.call' info mgr req

callWithResponse :: (MonadResource m, FromJSON responseType)
                 => APIRequest apiName responseType
                 -> TW m (Response responseType)
callWithResponse = callWithResponse'

callWithResponse' :: (MonadResource m, FromJSON value)
                  => APIRequest apiName responseType
                  -> TW m (Response value)
callWithResponse' req = do
    TWEnv info mgr <- ask
    lift $ Base.callWithResponse' info mgr req

sourceWithMaxId :: ( MonadResource m
                   , FromJSON responseType
                   , AsStatus responseType
                   , HasMaxIdParam (APIRequest apiName [responseType])
                   )
                => APIRequest apiName [responseType]
                -> C.Source (TW m) responseType
sourceWithMaxId req = do
    TWEnv info mgr <- lift ask
    Base.sourceWithMaxId info mgr req

sourceWithMaxId' :: ( MonadResource m
                    , HasMaxIdParam (APIRequest apiName [responseType])
                    )
                 => APIRequest apiName [responseType]
                 -> C.Source (TW m) Value
sourceWithMaxId' req = do
    TWEnv info mgr <- lift ask
    Base.sourceWithMaxId' info mgr req

sourceWithCursor :: ( MonadResource m
                    , FromJSON responseType
                    , CursorKey ck
                    , HasCursorParam (APIRequest apiName (WithCursor ck responseType))
                    )
                 => APIRequest apiName (WithCursor ck responseType)
                 -> C.Source (TW m) responseType
sourceWithCursor req = do
    TWEnv info mgr <- lift ask
    Base.sourceWithCursor info mgr req

sourceWithCursor' :: ( MonadResource m
                     , FromJSON responseType
                     , CursorKey ck
                     , HasCursorParam (APIRequest apiName (WithCursor ck responseType))
                     )
                  => APIRequest apiName (WithCursor ck responseType)
                  -> C.Source (TW m) Value
sourceWithCursor' req = do
    TWEnv info mgr <- lift ask
    Base.sourceWithCursor' info mgr req
