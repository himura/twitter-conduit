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
       , FilterParameter (..)
       , statusesFilter
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

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.Char
import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HTTP

#if MIN_VERSION_conduit(1,3,0)
#else
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
#endif

stream ::
          ( MonadResource m
          , FromJSON responseType
#if MIN_VERSION_conduit(1,3,0)
          , MonadThrow m
#endif
          )
       => TWInfo
       -> HTTP.Manager
       -> APIRequest apiName responseType
#if MIN_VERSION_http_conduit(2,3,0)
        -> m (C.ConduitM () responseType m ())
#else
       -> m (C.ResumableSource m responseType)
#endif
stream = stream'

stream' ::
           ( MonadResource m
           , FromJSON value
#if MIN_VERSION_conduit(1,3,0)
           , MonadThrow m
#endif
           )
        => TWInfo
        -> HTTP.Manager
        -> APIRequest apiName responseType
#if MIN_VERSION_http_conduit(2,3,0)
        -> m (C.ConduitM () value m ())
#else
        -> m (C.ResumableSource m value)
#endif
stream' info mgr req = do
    rsrc <- getResponse info mgr =<< liftIO (makeRequest req)
#if MIN_VERSION_http_conduit(2,3,0)
    return $ responseBody rsrc C..| CL.sequence sinkFromJSONIgnoreSpaces
#else
    responseBody rsrc $=+ CL.sequence sinkFromJSONIgnoreSpaces
#endif
  where
    sinkFromJSONIgnoreSpaces = CL.filter (not . S8.all isSpace) C.=$ sinkFromJSON

data Userstream
userstream :: APIRequest Userstream StreamingAPI
userstream = APIRequestGet "https://userstream.twitter.com/1.1/user.json" []
deriveHasParamInstances ''Userstream
    [ "language"
    , "filter_level"
    , "stall_warnings"
    , "replies"
    ]

-- https://dev.twitter.com/streaming/overview/request-parameters
data FilterParameter = Follow [UserId]
                     | Track [T.Text]

-- | Returns statuses/filter.json API query data.
--
-- >>> statusesFilter [Follow [1,2,3]]
-- APIRequestPost "https://stream.twitter.com/1.1/statuses/filter.json" [("follow","1,2,3")]
-- >>> statusesFilter [Track ["haskell","functional"]]
-- APIRequestPost "https://stream.twitter.com/1.1/statuses/filter.json" [("track","haskell,functional")]
-- >>> statusesFilter [Follow [1,2,3],Track ["haskell","functional"]]
-- APIRequestPost "https://stream.twitter.com/1.1/statuses/filter.json" [("follow","1,2,3"),("track","haskell,functional")]
statusesFilter :: [FilterParameter] -> APIRequest StatusesFilter StreamingAPI
statusesFilter fs = APIRequestPost statusesFilterEndpoint (L.map paramToQueryItem fs) Nothing

paramToQueryItem :: FilterParameter -> APIQueryItem
paramToQueryItem (Follow userIds) = ("follow", PVIntegerArray userIds)
paramToQueryItem (Track texts) = ("track", PVStringArray texts)

statusesFilterEndpoint :: String
statusesFilterEndpoint = "https://stream.twitter.com/1.1/statuses/filter.json"

data StatusesFilter

-- | Returns statuses/filter.json API query data.
--
-- >>> statusesFilterByFollow [1,2,3]
-- APIRequestPost "https://stream.twitter.com/1.1/statuses/filter.json" [("follow","1,2,3")]
statusesFilterByFollow :: [UserId] -> APIRequest StatusesFilter StreamingAPI
statusesFilterByFollow userIds = statusesFilter [Follow userIds]

-- | Returns statuses/filter.json API query data.
--
-- >>> statusesFilterByTrack "haskell"
-- APIRequestPost "https://stream.twitter.com/1.1/statuses/filter.json" [("track","haskell")]
statusesFilterByTrack :: T.Text -- ^ keyword
                      -> APIRequest StatusesFilter StreamingAPI
statusesFilterByTrack keyword = statusesFilter [Track [keyword]]

deriveHasParamInstances ''StatusesFilter
    [ "language"
    , "filter_level"
    , "stall_warnings"
    ]
