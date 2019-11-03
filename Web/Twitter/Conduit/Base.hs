{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Twitter.Conduit.Base
       ( ResponseBodyType (..)
       , NoContent
       , getResponse
       , call
       , call'
       , callWithResponse
       , callWithResponse'
       , checkResponse
       , sourceWithMaxId
       , sourceWithMaxId'
       , sourceWithCursor
       , sourceWithCursor'
       , sourceWithSearchResult
       , sourceWithSearchResult'
       , endpoint
       , makeRequest
       , sinkJSON
       , sinkFromJSON
       ) where

import Web.Twitter.Conduit.Cursor
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Request.Internal
import Web.Twitter.Conduit.Response
import Web.Twitter.Conduit.Types
import Web.Twitter.Types.Lens

import Control.Lens
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Coerce
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HT
import Web.Authenticate.OAuth (signOAuth)

#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif

makeRequest :: APIRequest apiName responseType
            -> IO HTTP.Request
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

makeRequest' :: HT.Method -- ^ HTTP request method (GET or POST)
             -> String -- ^ API Resource URL
             -> HT.SimpleQuery -- ^ Query
             -> IO HTTP.Request
makeRequest' m url query = do
    req <- HTTP.parseRequest url
    let addParams =
            if m == "POST"
            then HTTP.urlEncodedBody query
            else \r -> r { HTTP.queryString = HT.renderSimpleQuery False query }
    return $ addParams $ req { HTTP.method = m }

class ResponseBodyType a where
    parseResponseBody ::
           Response (C.ConduitM () ByteString (ResourceT IO) ())
        -> ResourceT IO (Response a)

type NoContent = ()
instance ResponseBodyType NoContent where
    parseResponseBody res =
        case responseStatus res of
            st | st == HT.status204 -> return $ void res
            _ -> do
                body <- C.runConduit $ responseBody res C..| sinkJSON
                throwM $ TwitterStatusError (responseStatus res) (responseHeaders res) body

instance {-# OVERLAPPABLE #-} FromJSON a => ResponseBodyType a where
    parseResponseBody = getValueOrThrow

getResponse :: MonadResource m
            => TWInfo
            -> HTTP.Manager
            -> HTTP.Request
            -> m (Response (C.ConduitM () ByteString m ()))
getResponse TWInfo{..} mgr req = do
    signedReq <- signOAuth (twOAuth twToken) (twCredential twToken) $ req { HTTP.proxy = twProxy }
    res <- HTTP.http signedReq mgr
    return
        Response { responseStatus = HTTP.responseStatus res
                 , responseHeaders = HTTP.responseHeaders res
                 , responseBody = HTTP.responseBody res
                 }

endpoint :: String
endpoint = "https://api.twitter.com/1.1/"

getValue ::
            Response (C.ConduitM () ByteString (ResourceT IO) ())
         -> ResourceT IO (Response Value)
getValue res = do
    value <-
      C.runConduit $ responseBody res C..| sinkJSON
    return $ res { responseBody = value }

checkResponse :: Response Value
              -> Either TwitterError Value
checkResponse Response{..} =
    case responseBody ^? key "errors" of
        Just errs@(Array _) ->
            case fromJSON errs of
                Success errList -> Left $ TwitterErrorResponse responseStatus responseHeaders errList
                Error msg -> Left $ FromJSONError msg
        Just err ->
            Left $ TwitterUnknownErrorResponse responseStatus responseHeaders err
        Nothing ->
            if sci < 200 || sci > 400
                then Left $ TwitterStatusError responseStatus responseHeaders responseBody
                else Right responseBody
  where
    sci = HT.statusCode responseStatus

getValueOrThrow :: FromJSON a
                => Response (C.ConduitM () ByteString (ResourceT IO) ())
                -> ResourceT IO (Response a)
getValueOrThrow res = do
    res' <- getValue res
    case checkResponse res' of
        Left err -> throwM err
        Right _ -> return ()
    case fromJSON (responseBody res') of
        Success r -> return $ res' { responseBody = r }
        Error err -> throwM $ FromJSONError err

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
call :: ResponseBodyType responseType
     => TWInfo -- ^ Twitter Setting
     -> HTTP.Manager
     -> APIRequest apiName responseType
     -> IO responseType
call = call'

-- | Perform an 'APIRequest' and then provide the response.
-- The response of this function is not restrict to @responseType@,
-- so you can choose an arbitrarily type of FromJSON instances.
call' :: ResponseBodyType value
      => TWInfo -- ^ Twitter Setting
      -> HTTP.Manager
      -> APIRequest apiName responseType
      -> IO value
call' info mgr req = responseBody `fmap` callWithResponse' info mgr req

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
callWithResponse :: ResponseBodyType responseType
                 => TWInfo -- ^ Twitter Setting
                 -> HTTP.Manager
                 -> APIRequest apiName responseType
                 -> IO (Response responseType)
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
callWithResponse' :: ResponseBodyType value
                  => TWInfo
                  -> HTTP.Manager
                  -> APIRequest apiName responseType
                  -> IO (Response value)
callWithResponse' info mgr req =
    runResourceT $ do
        res <- getResponse info mgr =<< liftIO (makeRequest req)
        parseResponseBody res

-- | A wrapper function to perform multiple API request with changing @max_id@ parameter.
--
-- This function cooperate with instances of 'HasMaxIdParam'.
sourceWithMaxId :: ( MonadIO m
                   , FromJSON responseType
                   , AsStatus responseType
                   , HasParam "max_id" Integer supports
                   )
                => TWInfo -- ^ Twitter Setting
                -> HTTP.Manager
                -> APIRequest supports [responseType]
                -> C.ConduitT () responseType m ()
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
sourceWithMaxId' :: ( MonadIO m
                    , HasParam "max_id" Integer supports
                    )
                 => TWInfo -- ^ Twitter Setting
                 -> HTTP.Manager
                 -> APIRequest supports [responseType]
                 -> C.ConduitT () Value m ()
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
sourceWithCursor :: ( MonadIO m
                    , FromJSON responseType
                    , CursorKey ck
                    , HasParam "cursor" Integer supports
                    )
                 => TWInfo -- ^ Twitter Setting
                 -> HTTP.Manager
                 -> APIRequest supports (WithCursor Integer ck responseType)
                 -> C.ConduitT () responseType m ()
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
sourceWithCursor' :: ( MonadIO m
                     , CursorKey ck
                     , HasParam "cursor" Integer supports
                     )
                  => TWInfo -- ^ Twitter Setting
                  -> HTTP.Manager
                  -> APIRequest supports (WithCursor Integer ck responseType)
                  -> C.ConduitT () Value m ()
sourceWithCursor' info mgr req = loop (Just (-1))
  where
    relax :: APIRequest apiName (WithCursor Integer ck responseType)
          -> APIRequest apiName (WithCursor Integer ck Value)
    relax = coerce
    loop Nothing = CL.sourceNull
    loop (Just 0) = CL.sourceNull
    loop (Just cur) = do
        res <- liftIO $ call info mgr $ relax $ req & #cursor ?~ cur
        CL.sourceList $ contents res
        loop $ nextCursor res

-- | A wrapper function to perform multiple API request with @SearchResult@.
sourceWithSearchResult :: ( MonadIO m
                          , FromJSON responseType
                          )
                       => TWInfo -- ^ Twitter Setting
                       -> HTTP.Manager
                       -> APIRequest supports (SearchResult [responseType])
                       -> m (SearchResult (C.ConduitT () responseType m ()))
sourceWithSearchResult info mgr req = do
    res <- liftIO $ call info mgr req
    let body = CL.sourceList (res ^. searchResultStatuses) <>
               loop (res ^. searchResultSearchMetadata . searchMetadataNextResults)
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
sourceWithSearchResult' :: ( MonadIO m
                           )
                        => TWInfo -- ^ Twitter Setting
                        -> HTTP.Manager
                        -> APIRequest supports (SearchResult [responseType])
                        -> m (SearchResult (C.ConduitT () Value m ()))
sourceWithSearchResult' info mgr req = do
    res <- liftIO $ call info mgr $ relax req
    let body = CL.sourceList (res ^. searchResultStatuses) <>
               loop (res ^. searchResultSearchMetadata . searchMetadataNextResults)
    return $ res & searchResultStatuses .~ body
  where
    origQueryMap = req ^. params . to M.fromList
    relax :: APIRequest apiName (SearchResult [responseType])
          -> APIRequest apiName (SearchResult [Value])
    relax = coerce
    loop Nothing = CL.sourceNull
    loop (Just nextResultsStr) = do
        let nextResults = nextResultsStr & HT.parseSimpleQuery . T.encodeUtf8 & traversed . _2 %~ (PVString . T.decodeUtf8)
            nextParams = M.toList $ M.union (M.fromList nextResults) origQueryMap
        res <- liftIO $ call info mgr $ relax $ req & params .~ nextParams
        CL.sourceList (res ^. searchResultStatuses)
        loop $ res ^. searchResultSearchMetadata . searchMetadataNextResults

sinkJSON :: ( MonadThrow m
            ) => C.ConduitT ByteString o m Value
sinkJSON = CA.sinkParser json

sinkFromJSON :: ( FromJSON a
                , MonadThrow m
                ) => C.ConduitT ByteString o m a
sinkFromJSON = do
    v <- sinkJSON
    case fromJSON v of
        Error err -> throwM $ FromJSONError err
        Success r -> return r
