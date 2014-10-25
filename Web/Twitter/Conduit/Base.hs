{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Twitter.Conduit.Base
       ( api
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
       , TwitterBaseM
       , endpoint
       , makeRequest
       , sinkJSON
       , sinkFromJSON
       , showBS
       ) where

import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Response
import Web.Twitter.Conduit.Cursor
import Web.Twitter.Types.Lens

import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Types as HT
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource, MonadThrow, monadThrow)
import Control.Monad.Logger
import Control.Lens
import Unsafe.Coerce

type TwitterBaseM m = ( MonadResource m
                      , MonadLogger m
                      )

makeRequest :: (MonadThrow m, MonadIO m)
            => APIRequest apiName responseType
            -> m HTTP.Request
makeRequest (APIRequestGet u pa) = makeRequest' "GET" u (makeSimpleQuery pa)
makeRequest (APIRequestPost u pa) = makeRequest' "POST" u (makeSimpleQuery pa)
makeRequest (APIRequestPostMultipart u param prt) =
    formDataBody body =<< makeRequest' "POST" u []
  where
    body = prt ++ partParam
    partParam = Prelude.map (uncurry partBS . over _1 T.decodeUtf8) (makeSimpleQuery param)

makeRequest' :: MonadThrow m
             => HT.Method -- ^ HTTP request method (GET or POST)
             -> String -- ^ API Resource URL
             -> HT.SimpleQuery -- ^ Query
             -> m HTTP.Request
makeRequest' m url query = do
    req <- HTTP.parseUrl url
    return $ req { HTTP.method = m
                 , HTTP.queryString = HT.renderSimpleQuery False query
                 , HTTP.checkStatus = \_ _ _ -> Nothing
                 }

{-# DEPRECATED api "use `getResponse =<< makeRequest'`" #-}
api :: TwitterBaseM m
    => HT.Method -- ^ HTTP request method (GET or POST)
    -> String -- ^ API Resource URL
    -> HT.SimpleQuery -- ^ Query
    -> TW m (Response (C.ResumableSource (TW m) ByteString))
api m url query =
    getResponse =<< makeRequest' m url query

getResponse :: TwitterBaseM m
            => HTTP.Request
            -> TW m (Response (C.ResumableSource (TW m) ByteString))
getResponse req = do
    proxy <- getProxy
    signedReq <- signOAuthTW $ req { HTTP.proxy = proxy }
    $(logDebug) $ T.pack $ "Signed Request: " ++ show signedReq
    mgr <- getManager
    res <- HTTP.http signedReq mgr
    $(logDebug) $ T.pack $ "Response Status: " ++ show (HTTP.responseStatus res)
    $(logDebug) $ T.pack $ "Response Header: " ++ show (HTTP.responseHeaders res)
    return
        Response { responseStatus = HTTP.responseStatus res
                 , responseHeaders = HTTP.responseHeaders res
                 , responseBody = HTTP.responseBody res
                 }

endpoint :: String
endpoint = "https://api.twitter.com/1.1/"

getValue :: (MonadLogger m, MonadThrow m)
         => Response (C.ResumableSource m ByteString)
         -> m (Response Value)
getValue res = do
    value <- responseBody res C.$$+- sinkJSON
    return $ res { responseBody = value }

checkResponse :: Response Value
              -> Either TwitterError Value
checkResponse Response{..} =
    case responseBody ^? key "errors" of
        Just errs ->
            case fromJSON errs of
                Success errList -> Left $ TwitterErrorResponse responseStatus responseHeaders errList
                Error msg -> Left $ FromJSONError msg
        Nothing ->
            if sci < 200 || sci > 400
                then Left $ TwitterStatusError responseStatus responseHeaders responseBody
                else Right responseBody
  where
    sci = HT.statusCode responseStatus

getValueOrThrow :: (MonadThrow m, MonadLogger m, FromJSON a)
                => Response (C.ResumableSource m ByteString)
                -> m (Response a)
getValueOrThrow res = do
    res' <- getValue res
    case checkResponse res' of
        Left err -> monadThrow err
        Right _ -> return ()
    case fromJSON (responseBody res') of
        Success r -> return $ res' { responseBody = r }
        Error err -> monadThrow $ FromJSONError err

call :: (TwitterBaseM m, FromJSON responseType)
     => APIRequest apiName responseType
     -> TW m responseType
call = call'

call' :: (TwitterBaseM m, FromJSON value)
      => APIRequest apiName responseType
      -> TW m value
call' = fmap responseBody . callWithResponse'

callWithResponse :: (TwitterBaseM m, FromJSON responseType)
                 => APIRequest apiName responseType
                 -> TW m (Response responseType)
callWithResponse = callWithResponse'

callWithResponse' :: (TwitterBaseM m, FromJSON value)
                  => APIRequest apiName responseType
                  -> TW m (Response value)
callWithResponse' req = getValueOrThrow =<< getResponse =<< makeRequest req

sourceWithMaxId :: ( TwitterBaseM m
                   , FromJSON responseType
                   , AsStatus responseType
                   , HasMaxIdParam (APIRequest apiName [responseType])
                   )
                => APIRequest apiName [responseType]
                -> C.Source (TW m) responseType
sourceWithMaxId = loop
  where
    loop req = do
        res <- lift $ call req
        case getMinId res of
            Just mid -> do
                CL.sourceList res
                loop $ req & maxId ?~ mid - 1
            Nothing -> CL.sourceList res
    getMinId = minimumOf (traverse . status_id)

sourceWithMaxId' :: ( TwitterBaseM m
                    , HasMaxIdParam (APIRequest apiName [responseType])
                    )
                 => APIRequest apiName [responseType]
                 -> C.Source (TW m) Value
sourceWithMaxId' = loop
  where
    loop req = do
        res <- lift $ call' req
        case getMinId res of
            Just mid -> do
                CL.sourceList res
                loop $ req & maxId ?~ mid - 1
            Nothing -> CL.sourceList res
    getMinId = minimumOf (traverse . key "id" . _Integer)

sourceWithCursor :: ( TwitterBaseM m
                    , FromJSON responseType
                    , CursorKey ck
                    , HasCursorParam (APIRequest apiName (WithCursor ck responseType))
                    )
                 => APIRequest apiName (WithCursor ck responseType)
                 -> C.Source (TW m) responseType
sourceWithCursor req = loop (-1)
  where
    loop 0 = CL.sourceNull
    loop cur = do
        res <- lift $ call $ req & cursor ?~ cur
        CL.sourceList $ contents res
        loop $ nextCursor res

sourceWithCursor' :: ( TwitterBaseM m
                     , FromJSON responseType
                     , CursorKey ck
                     , HasCursorParam (APIRequest apiName (WithCursor ck responseType))
                     )
                  => APIRequest apiName (WithCursor ck responseType)
                  -> C.Source (TW m) Value
sourceWithCursor' req = loop (-1)
  where
    relax :: FromJSON value
          => APIRequest apiName (WithCursor ck responseType)
          -> APIRequest apiName (WithCursor ck value)
    relax = unsafeCoerce
    loop 0 = CL.sourceNull
    loop cur = do
        res <- lift $ call $ relax $ req & cursor ?~ cur
        CL.sourceList $ contents res
        loop $ nextCursor res

sinkJSON :: ( MonadThrow m
            , MonadLogger m
            ) => C.Consumer ByteString m Value
sinkJSON = do
    js <- CA.sinkParser json
    $(logDebug) $ T.pack $ "Response JSON: " ++ show js
    return js

sinkFromJSON :: ( FromJSON a
                , MonadThrow m
                , MonadLogger m
                ) => C.Consumer ByteString m a
sinkFromJSON = do
    v <- sinkJSON
    case fromJSON v of
        Error err -> monadThrow $ FromJSONError err
        Success r -> return r

showBS :: Show a => a -> ByteString
showBS = S8.pack . show
