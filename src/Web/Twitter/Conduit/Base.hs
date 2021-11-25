{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Twitter.Conduit.Base (
    ResponseBodyType (..),
    call,
    call',
    callWithResponse,
    callWithResponse',
    sourceWithMaxId,
    sourceWithMaxId',
    sourceWithCursor,
    sourceWithCursor',
    sourceWithSearchResult,
    sourceWithSearchResult',
    endpoint,
    makeRequest
) where

import Web.Twitter.Conduit.Cursor
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Request.Internal
import Web.Twitter.Conduit.Response
import Web.Twitter.Conduit.Types
import Web.Twitter.Types.Lens

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Coerce
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import GHC.TypeLits (KnownSymbol)
import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HT
import Web.Authenticate.OAuth (signOAuth)

makeRequest ::
    APIRequest apiName responseType ->
    IO HTTP.Request
makeRequest (APIRequest m u pa) = makeRequest' m u (makeSimpleQuery pa)
makeRequest (APIRequestMultipart m u param prt) =
    formDataBody body =<< makeRequest' m u []
  where
    body = prt ++ partParam
    partParam = Prelude.map (uncurry partBS . over _1 T.decodeUtf8) (makeSimpleQuery param)
makeRequest (APIRequestJSON m u param body) = do
    req <- makeRequest' m u (makeSimpleQuery param)
    return $
        req
            { HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
            , HTTP.requestHeaders = ("Content-Type", "application/json") : HTTP.requestHeaders req
            }

makeRequest' ::
    -- | HTTP request method (GET or POST)
    HT.Method ->
    -- | API Resource URL
    String ->
    -- | Query
    HT.SimpleQuery ->
    IO HTTP.Request
makeRequest' m url query = do
    req <- HTTP.parseRequest url
    let addParams =
            if m == "POST"
                then HTTP.urlEncodedBody query
                else \r -> r {HTTP.queryString = HT.renderSimpleQuery False query}
    return $ addParams $ req {HTTP.method = m}

withHTTPResponse ::
       TWInfo
    -> HTTP.Manager
    -> HTTP.Request
    -> (HTTP.Response HTTP.BodyReader -> IO a)
    -> IO a
withHTTPResponse TWInfo {..} mgr req respond = do
    signedReq <- signOAuth (twOAuth twToken) (twCredential twToken) $ req {HTTP.proxy = twProxy}
    HTTP.withResponse signedReq mgr respond

endpoint :: String
endpoint = "https://api.twitter.com/1.1/"

-- | Perform an 'APIRequest' and then provide the response which is mapped to a suitable type of
-- <http://hackage.haskell.org/package/twitter-types twitter-types>.
--
-- Example:
--
-- @
-- user <- 'call' twInfo mgr $ 'accountVerifyCredentials'
-- print user
-- @
--
-- If you need raw JSON value which is parsed by <http://hackage.haskell.org/package/aeson aeson>,
-- use 'call'' to obtain it.
call ::
    ResponseBodyType responseType =>
    -- | Twitter Setting
    TWInfo ->
    HTTP.Manager ->
    APIRequest apiName responseType ->
    IO responseType
call = call'

-- | Perform an 'APIRequest' and then provide the response.
-- The response of this function is not restrict to @responseType@,
-- so you can choose an arbitrarily type of FromJSON instances.
call' ::
    ResponseBodyType value =>
    -- | Twitter Setting
    TWInfo ->
    HTTP.Manager ->
    APIRequest apiName responseType ->
    IO value
call' info mgr req = apiResponseBody `fmap` callWithResponse' info mgr req

-- | Perform an 'APIRequest' and then provide the 'Response'.
--
-- Example:
--
-- @
-- res \<- 'callWithResponse' twInfo mgr $ 'accountVerifyCredentials'
-- 'print' $ 'responseStatus' res
-- 'print' $ 'responseHeaders' res
-- 'print' $ 'responseBody' res
-- @
callWithResponse ::
    ResponseBodyType responseType =>
    -- | Twitter Setting
    TWInfo ->
    HTTP.Manager ->
    APIRequest apiName responseType ->
    IO (APIResponse responseType)
callWithResponse = callWithResponse'

-- | Perform an 'APIRequest' and then provide the 'Response'.
-- The response of this function is not restrict to @responseType@,
-- so you can choose an arbitrarily type of FromJSON instances.
--
-- Example:
--
-- @
-- res \<- 'callWithResponse'' twInfo mgr $ 'accountVerifyCredentials'
-- 'print' $ 'responseStatus' res
-- 'print' $ 'responseHeaders' res
-- 'print' $ 'responseBody' (res :: Value)
-- @
callWithResponse' ::
    ResponseBodyType value =>
    TWInfo ->
    HTTP.Manager ->
    APIRequest apiName responseType ->
    IO (APIResponse value)
callWithResponse' info mgr req = do
    httpReq <- makeRequest req
    withHTTPResponse info mgr httpReq handleResponseThrow

-- | A wrapper function to perform multiple API request with changing @max_id@ parameter.
--
-- This function cooperate with instances of 'HasMaxIdParam'.
sourceWithMaxId ::
    ( MonadIO m
    , FromJSON responseType
    , AsStatus responseType
    , HasParam "max_id" Integer supports
    ) =>
    -- | Twitter Setting
    TWInfo ->
    HTTP.Manager ->
    APIRequest supports [responseType] ->
    C.ConduitT () responseType m ()
sourceWithMaxId info mgr = loop
  where
    loop req = do
        res <- liftIO $ call info mgr req
        case getMinId res of
            Just mid -> do
                CL.sourceList res
                loop $ req & #max_id ?~ mid - 1
            Nothing -> CL.sourceList res
    getMinId = minimumOf (traverse . status_id)

-- | A wrapper function to perform multiple API request with changing @max_id@ parameter.
-- The response of this function is not restrict to @responseType@,
-- so you can choose an arbitrarily type of FromJSON instances.
--
-- This function cooperate with instances of 'HasMaxIdParam'.
sourceWithMaxId' ::
    ( MonadIO m
    , HasParam "max_id" Integer supports
    ) =>
    -- | Twitter Setting
    TWInfo ->
    HTTP.Manager ->
    APIRequest supports [responseType] ->
    C.ConduitT () Value m ()
sourceWithMaxId' info mgr = loop
  where
    loop req = do
        (res :: [Value]) <- liftIO $ call' info mgr req
        case minimumOf (traverse . key "id" . _Integer) res of
            Just mid -> do
                CL.sourceList res
                loop $ req & #max_id ?~ mid - 1
            Nothing -> CL.sourceList res

-- | A wrapper function to perform multiple API request with changing @cursor@ parameter.
--
-- This function cooperate with instances of 'HasCursorParam'.
sourceWithCursor ::
    ( MonadIO m
    , FromJSON responseType
    , KnownSymbol ck
    , HasParam "cursor" Integer supports
    ) =>
    -- | Twitter Setting
    TWInfo ->
    HTTP.Manager ->
    APIRequest supports (WithCursor Integer ck responseType) ->
    C.ConduitT () responseType m ()
sourceWithCursor info mgr req = loop (Just (-1))
  where
    loop Nothing = CL.sourceNull
    loop (Just 0) = CL.sourceNull
    loop (Just cur) = do
        res <- liftIO $ call info mgr $ req & #cursor ?~ cur
        CL.sourceList $ contents res
        loop $ nextCursor res

-- | A wrapper function to perform multiple API request with changing @cursor@ parameter.
-- The response of this function is not restrict to @responseType@,
-- so you can choose an arbitrarily type of FromJSON instances.
--
-- This function cooperate with instances of 'HasCursorParam'.
sourceWithCursor' ::
    ( MonadIO m
    , KnownSymbol ck
    , HasParam "cursor" Integer supports
    ) =>
    -- | Twitter Setting
    TWInfo ->
    HTTP.Manager ->
    APIRequest supports (WithCursor Integer ck responseType) ->
    C.ConduitT () Value m ()
sourceWithCursor' info mgr req = loop (Just (-1))
  where
    relax ::
        APIRequest apiName (WithCursor Integer ck responseType) ->
        APIRequest apiName (WithCursor Integer ck Value)
    relax = coerce
    loop Nothing = CL.sourceNull
    loop (Just 0) = CL.sourceNull
    loop (Just cur) = do
        res <- liftIO $ call info mgr $ relax $ req & #cursor ?~ cur
        CL.sourceList $ contents res
        loop $ nextCursor res

-- | A wrapper function to perform multiple API request with @SearchResult@.
sourceWithSearchResult ::
    ( MonadIO m
    , FromJSON responseType
    ) =>
    -- | Twitter Setting
    TWInfo ->
    HTTP.Manager ->
    APIRequest supports (SearchResult [responseType]) ->
    m (SearchResult (C.ConduitT () responseType m ()))
sourceWithSearchResult info mgr req = do
    res <- liftIO $ call info mgr req
    let body =
            CL.sourceList (res ^. searchResultStatuses)
                <> loop (res ^. searchResultSearchMetadata . searchMetadataNextResults)
    return $ res & searchResultStatuses .~ body
  where
    origQueryMap = req ^. params . to M.fromList
    loop Nothing = CL.sourceNull
    loop (Just nextResultsStr) = do
        let nextResults = nextResultsStr & HT.parseSimpleQuery . T.encodeUtf8 & traversed . _2 %~ (PVString . T.decodeUtf8)
            nextParams = M.toList $ M.union (M.fromList nextResults) origQueryMap
        res <- liftIO $ call info mgr $ req & params .~ nextParams
        CL.sourceList (res ^. searchResultStatuses)
        loop $ res ^. searchResultSearchMetadata . searchMetadataNextResults

-- | A wrapper function to perform multiple API request with @SearchResult@.
sourceWithSearchResult' ::
    ( MonadIO m
    ) =>
    -- | Twitter Setting
    TWInfo ->
    HTTP.Manager ->
    APIRequest supports (SearchResult [responseType]) ->
    m (SearchResult (C.ConduitT () Value m ()))
sourceWithSearchResult' info mgr req = do
    res <- liftIO $ call info mgr $ relax req
    let body =
            CL.sourceList (res ^. searchResultStatuses)
                <> loop (res ^. searchResultSearchMetadata . searchMetadataNextResults)
    return $ res & searchResultStatuses .~ body
  where
    origQueryMap = req ^. params . to M.fromList
    relax ::
        APIRequest apiName (SearchResult [responseType]) ->
        APIRequest apiName (SearchResult [Value])
    relax = coerce
    loop Nothing = CL.sourceNull
    loop (Just nextResultsStr) = do
        let nextResults = nextResultsStr & HT.parseSimpleQuery . T.encodeUtf8 & traversed . _2 %~ (PVString . T.decodeUtf8)
            nextParams = M.toList $ M.union (M.fromList nextResults) origQueryMap
        res <- liftIO $ call info mgr $ relax $ req & params .~ nextParams
        CL.sourceList (res ^. searchResultStatuses)
        loop $ res ^. searchResultSearchMetadata . searchMetadataNextResults
