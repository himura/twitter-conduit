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
    responseBody rsrc $=+ CL.sequence sinkFromJSONIgnoreSpaces
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
statusesFilter = APIRequestPost statusesFilterEndpoint . L.map paramToQueryItem

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
