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

import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Monad
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

stream :: (TwitterBaseM m, FromJSON responseType)
       => APIRequest apiName responseType
       -> TW m (C.ResumableSource (TW m) responseType)
stream = stream'

stream' :: (TwitterBaseM m, FromJSON value)
        => APIRequest apiName responseType
        -> TW m (C.ResumableSource (TW m) value)
stream' req = do
    rsrc <- getResponse =<< makeRequest req
    responseBody rsrc $=+ CL.sequence sinkFromJSON

data Userstream
userstream :: APIRequest Userstream StreamingAPI
userstream = APIRequestGet "https://userstream.twitter.com/1.1/user.json" []
deriveHasParamInstances ''Userstream
    [ "language"
    , "filter_level"
    , "stall_warnings"
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
