{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Web.Twitter.Conduit.Internal where

import Control.Lens (Field1 (_1), over)
import Data.Aeson (ToJSON, encode)
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
import Web.Twitter.Conduit.Internal.APIResponse (APIResponse, ResponseBodyType, handleResponseThrow)
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

call ::
    (ToRequest body, ResponseBodyType responseType) =>
    TWInfo ->
    Manager ->
    APIRequest parameters body responseType ->
    IO (APIResponse responseType)
call = call'

call' ::
    (ToRequest body, ResponseBodyType value) =>
    TWInfo ->
    Manager ->
    APIRequest parameters body responseType ->
    IO (APIResponse value)
call' info mgr apiReq = do
    req <- buildHTTPRequest apiReq
    withHTTPResponse info mgr req handleResponseThrow
