{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Stream
       (
       -- * StreamingAPI
         userstream
       , userstream'
       , statusesFilter
       ) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Monad
import Web.Twitter.Types
import Web.Twitter.Conduit.Utils

import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import qualified Network.HTTP.Types as HT
import Control.Monad.IO.Class
import Data.Aeson

($=+) :: MonadIO m
      => CI.ResumableSource m a
      -> CI.Conduit a m o
      -> m (CI.ResumableSource m o)
rsrc $=+ cndt = do
    (src, finalizer) <- C.unwrapResumable rsrc
    return $ CI.ResumableSource (src C.$= cndt) finalizer

userstream :: TwitterBaseM m => TW m (C.ResumableSource (TW m) StreamingAPI)
userstream = userstream'

userstream' :: (TwitterBaseM m, FromJSON v) => TW m (C.ResumableSource (TW m) v)
userstream' = do
    rsrc <- api "GET" "https://userstream.twitter.com/1.1/user.json" []
    rsrc $=+ conduitFromJSON

statusesFilter :: TwitterBaseM m => HT.SimpleQuery -> TW m (C.ResumableSource (TW m) StreamingAPI)
statusesFilter query = do
    rsrc <- api "GET" "https://stream.twitter.com/1.1/statuses/filter.json" query
    rsrc $=+ conduitFromJSON
