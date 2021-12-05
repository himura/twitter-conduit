{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

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

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson (FromJSON, Value, json)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char (isSpace)
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HTTP
import Web.Authenticate.OAuth (signOAuth)
import Web.Twitter.Conduit.Internal (ToRequest (..))
import Web.Twitter.Conduit.Internal.APIRequest (
    APIQueryItem,
    APIRequest (..),
    BodyEmpty,
    Method (..),
    PV (PVIntegerArray, PVStringArray),
    Param ((:=)),
 )
import Web.Twitter.Conduit.Internal.APIResponse (APIException (..), eitherFromJSON)
import Web.Twitter.Conduit.Types (TWInfo (..), TWToken (TWToken, twCredential, twOAuth))
import Web.Twitter.Types (StreamingAPI, UserId)

stream ::
    ( MonadResource m
    , MonadThrow m
    , FromJSON responseType
    , ToRequest requestBody
    ) =>
    TWInfo ->
    HTTP.Manager ->
    APIRequest supports requestBody responseType ->
    m (C.ConduitM () responseType m ())
stream = stream'

stream' ::
    ( MonadResource m
    , MonadThrow m
    , FromJSON value
    , ToRequest requestBody
    ) =>
    TWInfo ->
    HTTP.Manager ->
    APIRequest supports requestBody responseType ->
    m (C.ConduitM () value m ())
stream' TWInfo {twToken = TWToken {twOAuth, twCredential}, twProxy} mgr apiReq = do
    req <- liftIO $ buildHTTPRequest apiReq
    signedReq <- signOAuth twOAuth twCredential $ req {HTTP.proxy = twProxy}
    res <- HTTP.http signedReq mgr
    return $ HTTP.responseBody res C..| CL.sequence (sinkFromJSONIgnoreSpaces res)
  where
    sinkFromJSONIgnoreSpaces res = CL.filter (not . S8.all isSpace) C..| sinkFromJSON res

userstream :: APIRequest Userstream BodyEmpty StreamingAPI
userstream = APIRequest GET "https://userstream.twitter.com/1.1/user.json" [] ()
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
statusesFilter :: [FilterParameter] -> APIRequest StatusesFilter BodyEmpty StreamingAPI
statusesFilter fs = APIRequest POST statusesFilterEndpoint (L.map paramToQueryItem fs) ()

paramToQueryItem :: FilterParameter -> APIQueryItem
paramToQueryItem (Follow userIds) = ("follow", PVIntegerArray userIds)
paramToQueryItem (Track texts) = ("track", PVStringArray texts)

statusesFilterEndpoint :: String
statusesFilterEndpoint = "https://stream.twitter.com/1.1/statuses/filter.json"

-- | Returns statuses/filter.json API query data.
--
-- >>> statusesFilterByFollow [1,2,3]
-- APIRequest "POST" "https://stream.twitter.com/1.1/statuses/filter.json" [("follow","1,2,3")]
statusesFilterByFollow :: [UserId] -> APIRequest StatusesFilter BodyEmpty StreamingAPI
statusesFilterByFollow userIds = statusesFilter [Follow userIds]

-- | Returns statuses/filter.json API query data.
--
-- >>> statusesFilterByTrack "haskell"
-- APIRequest "POST" "https://stream.twitter.com/1.1/statuses/filter.json" [("track","haskell")]
statusesFilterByTrack ::
    -- | keyword
    T.Text ->
    APIRequest StatusesFilter BodyEmpty StreamingAPI
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
