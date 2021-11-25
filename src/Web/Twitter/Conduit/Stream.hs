{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Twitter.Conduit.Stream (
    -- * StreamingAPI
    Userstream,
    userstream,
    StatusesFilter,
    FilterParameter (..),
    statusesFilter,
    statusesFilterByFollow,
    statusesFilterByTrack,
    -- , statusesFilterByLocation
    -- , statusesSample
    -- , statusesFirehose
    -- , sitestream
    -- , sitestream'
    stream,
    stream',
) where

import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Request.Internal
import Web.Twitter.Conduit.Response
import Web.Twitter.Conduit.Types
import Web.Twitter.Types

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HTTP
import Web.Authenticate.OAuth (signOAuth)

stream ::
    ( MonadResource m
    , FromJSON responseType
    , MonadThrow m
    ) =>
    TWInfo ->
    HTTP.Manager ->
    APIRequest apiName responseType ->
    m (C.ConduitM () responseType m ())
stream = stream'

stream' ::
    ( MonadResource m
    , FromJSON value
    , MonadThrow m
    ) =>
    TWInfo ->
    HTTP.Manager ->
    APIRequest apiName responseType ->
    m (C.ConduitM () value m ())
stream' TWInfo {twToken = TWToken {twOAuth, twCredential}, twProxy} mgr apiReq = do
    req <- liftIO (makeRequest apiReq)
    signedReq <- signOAuth twOAuth twCredential $ req {HTTP.proxy = twProxy}
    res <- HTTP.http signedReq mgr
    return $ HTTP.responseBody res C..| CL.sequence (sinkFromJSONIgnoreSpaces res)
  where
    sinkFromJSONIgnoreSpaces res =
        CL.filter (not . S8.all isSpace) C..| sinkFromJSON res

userstream :: APIRequest Userstream StreamingAPI
userstream = APIRequest "GET" "https://userstream.twitter.com/1.1/user.json" []
type Userstream =
    '[ "language" ':= T.Text
     , "filter_level" ':= T.Text
     , "stall_warnings" ':= Bool
     , "replies" ':= T.Text
     ]

-- https://dev.twitter.com/streaming/overview/request-parameters
data FilterParameter
    = Follow [UserId]
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
statusesFilterByTrack ::
    -- | keyword
    T.Text ->
    APIRequest StatusesFilter StreamingAPI
statusesFilterByTrack keyword = statusesFilter [Track [keyword]]

type StatusesFilter =
    '[ "language" ':= T.Text
     , "filter_level" ':= T.Text
     , "stall_warnings" ':= Bool
     ]

sinkJSON :: MonadThrow m => C.ConduitT S.ByteString o m Value
sinkJSON = CA.sinkParser json

sinkFromJSON ::
       (FromJSON a, MonadThrow m) => HTTP.Response body -> C.ConduitT S.ByteString o m a
sinkFromJSON res = do
    v <- sinkJSON
    case eitherFromJSON v of
        Left err ->
            throwM $
            APIException
                (HTTP.responseStatus res)
                (HTTP.responseHeaders res)
                err
        Right value -> return value
