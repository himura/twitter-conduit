{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Request.Internal
import Web.Twitter.Conduit.Response

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.Char
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HTTP

stream ::
          ( MonadResource m
          , FromJSON responseType
          , MonadThrow m
          )
       => TWInfo
       -> HTTP.Manager
       -> APIRequest apiName responseType
        -> m (C.ConduitM () responseType m ())
stream = stream'

stream' ::
           ( MonadResource m
           , FromJSON value
           , MonadThrow m
           )
        => TWInfo
        -> HTTP.Manager
        -> APIRequest apiName responseType
        -> m (C.ConduitM () value m ())
stream' info mgr req = do
    rsrc <- getResponse info mgr =<< liftIO (makeRequest req)
    return $ responseBody rsrc C..| CL.sequence sinkFromJSONIgnoreSpaces
  where
    sinkFromJSONIgnoreSpaces = CL.filter (not . S8.all isSpace) C..| sinkFromJSON

userstream :: APIRequest Userstream StreamingAPI
userstream = APIRequest "GET" "https://userstream.twitter.com/1.1/user.json" []
type Userstream = '[
      "language" ':= T.Text
    , "filter_level" ':= T.Text
    , "stall_warnings" ':= Bool
    , "replies" ':= T.Text
    ]

-- https://dev.twitter.com/streaming/overview/request-parameters
data FilterParameter = Follow [UserId]
                     | Track [T.Text]

-- | Returns statuses/filter.json API query data.
--
-- >>> statusesFilter [Follow [1,2,3]]
-- APIRequest "POST" "https://stream.twitter.com/1.1/statuses/filter.json" [("follow","1,2,3")]
-- >>> statusesFilter [Track ["haskell","functional"]]
-- APIRequest "POST" "https://stream.twitter.com/1.1/statuses/filter.json" [("track","haskell,functional")]
-- >>> statusesFilter [Follow [1,2,3],Track ["haskell","functional"]]
-- APIRequest "POST" "https://stream.twitter.com/1.1/statuses/filter.json" [("follow","1,2,3"),("track","haskell,functional")]
statusesFilter :: [FilterParameter] -> APIRequest StatusesFilter StreamingAPI
statusesFilter fs = APIRequest "POST" statusesFilterEndpoint (L.map paramToQueryItem fs)

paramToQueryItem :: FilterParameter -> APIQueryItem
paramToQueryItem (Follow userIds) = ("follow", PVIntegerArray userIds)
paramToQueryItem (Track texts) = ("track", PVStringArray texts)

statusesFilterEndpoint :: String
statusesFilterEndpoint = "https://stream.twitter.com/1.1/statuses/filter.json"

-- | Returns statuses/filter.json API query data.
--
-- >>> statusesFilterByFollow [1,2,3]
-- APIRequest "POST" "https://stream.twitter.com/1.1/statuses/filter.json" [("follow","1,2,3")]
statusesFilterByFollow :: [UserId] -> APIRequest StatusesFilter StreamingAPI
statusesFilterByFollow userIds = statusesFilter [Follow userIds]

-- | Returns statuses/filter.json API query data.
--
-- >>> statusesFilterByTrack "haskell"
-- APIRequest "POST" "https://stream.twitter.com/1.1/statuses/filter.json" [("track","haskell")]
statusesFilterByTrack :: T.Text -- ^ keyword
                      -> APIRequest StatusesFilter StreamingAPI
statusesFilterByTrack keyword = statusesFilter [Track [keyword]]
type StatusesFilter = '[
      "language" ':= T.Text
    , "filter_level" ':= T.Text
    , "stall_warnings" ':= Bool
    ]
