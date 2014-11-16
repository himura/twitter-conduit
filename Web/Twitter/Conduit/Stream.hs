{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Twitter.Conduit.Stream
       (
       -- * StreamingAPI
         Userstream
       , userstream
       , StatusesFilter
       , statusesFilterByFollow
       , statusesFilterByTrack
       -- , statusesFilterByLocation
       -- , statusesSample
       -- , statusesFirehose
       -- , sitestream
       -- , sitestream'
       , stream
       , stream'
  ) where

import Web.Twitter.Conduit.Types
import Web.Twitter.Conduit.Base
import Web.Twitter.Types
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Response

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Internal as CI
import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Aeson
import Control.Monad.Trans.Resource (MonadResource)
import qualified Network.HTTP.Conduit as HTTP

#if MIN_VERSION_conduit(1,0,16)
($=+) :: MonadIO m
      => CI.ResumableSource m a
      -> CI.Conduit a m o
      -> m (CI.ResumableSource m o)
($=+) = (return .) . (C.$=+)
#else
rsrc $=+ cndt = do
    (src, finalizer) <- C.unwrapResumable rsrc
    return $ CI.ResumableSource (src C.$= cndt) finalizer
#endif

stream :: (MonadResource m, FromJSON responseType)
       => TWInfo
       -> HTTP.Manager
       -> APIRequest apiName responseType
       -> m (C.ResumableSource m responseType)
stream = stream'

stream' :: (MonadResource m, FromJSON value)
        => TWInfo
        -> HTTP.Manager
        -> APIRequest apiName responseType
        -> m (C.ResumableSource m value)
stream' info mgr req = do
    rsrc <- getResponse info mgr =<< liftIO (makeRequest req)
    responseBody rsrc $=+ CL.sequence sinkFromJSON

data Userstream
userstream :: APIRequest Userstream StreamingAPI
userstream = APIRequestGet "https://userstream.twitter.com/1.1/user.json" []
deriveHasParamInstances ''Userstream
    [ "language"
    , "filter_level"
    , "stall_warnings"
    , "replies"
    ]

statusesFilterEndpoint :: String
statusesFilterEndpoint = "https://stream.twitter.com/1.1/statuses/filter.json"

data StatusesFilter
statusesFilterByFollow :: [UserId] -> APIRequest StatusesFilter StreamingAPI
statusesFilterByFollow userIds =
    APIRequestPost statusesFilterEndpoint [("follow", PVIntegerArray userIds)]

statusesFilterByTrack :: T.Text -- ^ keyword
                      -> APIRequest StatusesFilter StreamingAPI
statusesFilterByTrack keyword =
    APIRequestPost statusesFilterEndpoint [("track", PVString keyword)]

deriveHasParamInstances ''StatusesFilter
    [ "language"
    , "filter_level"
    , "stall_warnings"
    ]
