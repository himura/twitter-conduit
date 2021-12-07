{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Web.Twitter.Conduit.Internal where

import Control.Lens (Field1 (_1), over)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as S
import qualified Data.Text.Encoding as T
import Network.HTTP.Client as HTTP (
    BodyReader,
    Manager,
    Request (method, proxy, queryString, requestBody, requestHeaders),
    RequestBody (RequestBodyLBS),
    Response,
    parseRequest,
    urlEncodedBody,
    withResponse,
 )
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS)
import qualified Network.HTTP.Types as HTTPTypes
import Web.Authenticate.OAuth (signOAuth)
import Web.Twitter.Conduit.Internal.APIRequest (
    APIQuery,
    APIRequest (APIRequest),
    BodyEmpty,
    BodyJSON (..),
    BodyMultipart (..),
    Method (POST),
    convertToHTTPMethod,
    makeSimpleQuery,
 )
import Web.Twitter.Conduit.Internal.APIResponse (APIResponse (apiResponseBody), ResponseBodyType, handleResponseThrow)
import Web.Twitter.Conduit.Internal.WithRawValue (WithRawValue)
import Web.Twitter.Conduit.Types (TWInfo (..), TWToken (twCredential, twOAuth))

class ToRequest body where
    buildHTTPRequest :: APIRequest parameters body responseType -> IO Request

instance ToRequest BodyEmpty where
    buildHTTPRequest (APIRequest m u p ()) = do
        req <- makeHTTPRequest m u
        return $
            case m of
                POST -> urlEncodedBody (makeSimpleQuery p) req
                _ -> req {queryString = buildQueryString p}

instance ToRequest BodyMultipart where
    buildHTTPRequest (APIRequest m u p (BodyMultipart ps)) =
        formDataBody ps' =<< makeHTTPRequest m u
      where
        ps' = ps ++ paramParts
        paramParts = map (uncurry partBS . over _1 T.decodeUtf8) (makeSimpleQuery p)
instance ToJSON a => ToRequest (BodyJSON a) where
    buildHTTPRequest (APIRequest m u p (BodyJSON b)) = do
        req <- makeHTTPRequest m u
        return $
            req
                { queryString = buildQueryString p
                , requestBody = RequestBodyLBS $ encode b
                , requestHeaders = ("Content-Type", "application/json") : requestHeaders req
                }

makeHTTPRequest ::
    -- | HTTP request method
    Method ->
    -- | API Resource URL
    String ->
    IO Request
makeHTTPRequest m u = do
    req <- parseRequest u
    return $ req {HTTP.method = convertToHTTPMethod m}

buildQueryString :: APIQuery -> S.ByteString
buildQueryString = HTTPTypes.renderSimpleQuery False . makeSimpleQuery

withHTTPResponse ::
    TWInfo ->
    Manager ->
    Request ->
    (Response BodyReader -> IO a) ->
    IO a
withHTTPResponse TWInfo {..} mgr req respond = do
    signedReq <- signOAuth (twOAuth twToken) (twCredential twToken) $ req {HTTP.proxy = twProxy}
    HTTP.withResponse signedReq mgr respond

-- | Perform an 'APIRequest' and then provide the response which is mapped to a suitable type of
-- <http://hackage.haskell.org/package/twitter-types twitter-types>.
--
-- Example:
--
-- @
-- response <- 'send' twInfo mgr $ 'statusesHomeTimeline'
-- @
--
-- If you need raw JSON value which is parsed by <http://hackage.haskell.org/package/aeson aeson>,
-- use 'send'' to obtain it.
send ::
    (ToRequest body, ResponseBodyType responseType) =>
    TWInfo ->
    Manager ->
    APIRequest parameters body responseType ->
    IO (APIResponse responseType)
send = send'

sendWithRawValue ::
    (ToRequest body, ResponseBodyType responseType, FromJSON responseType) =>
    TWInfo ->
    Manager ->
    APIRequest parameters body responseType ->
    IO (APIResponse (WithRawValue responseType))
sendWithRawValue = send'

-- | Perform an 'APIRequest' and then provide the response.
-- The response of this function is not restrict to @responseType@,
-- so you can choose an arbitrarily type of FromJSON instances.
send' ::
    (ToRequest body, ResponseBodyType value) =>
    TWInfo ->
    Manager ->
    APIRequest parameters body responseType ->
    IO (APIResponse value)
send' info mgr apiReq = do
    req <- buildHTTPRequest apiReq
    withHTTPResponse info mgr req handleResponseThrow

callWithResponse ::
    (ToRequest body, ResponseBodyType responseType) =>
    TWInfo ->
    Manager ->
    APIRequest parameters body responseType ->
    IO (APIResponse responseType)
callWithResponse = send
{-# DEPRECATED callWithResponse "Please use Web.Twitter.Conduit.send" #-}

callWithResponse' ::
    (ToRequest body, ResponseBodyType value) =>
    TWInfo ->
    Manager ->
    APIRequest parameters body responseType ->
    IO (APIResponse value)
callWithResponse' = send'
{-# DEPRECATED callWithResponse' "Please use Web.Twitter.Conduit.send'" #-}

call ::
    (ToRequest body, ResponseBodyType responseType) =>
    TWInfo ->
    Manager ->
    APIRequest parameters body responseType ->
    IO responseType
call = call'

call' ::
    (ToRequest body, ResponseBodyType value) =>
    TWInfo ->
    Manager ->
    APIRequest parameters body responseType ->
    IO value
call' info mgr apiReq = apiResponseBody <$> send' info mgr apiReq
