{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Twitter.Conduit.Base
       ( getResponse
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

import Web.Twitter.Conduit.Cursor
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Response
import Web.Twitter.Conduit.Types
import Web.Twitter.Types.Lens

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource, MonadThrow, monadThrow)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HT
import Unsafe.Coerce
import Web.Authenticate.OAuth (signOAuth)

type TwitterBaseM m = ( MonadResource m
                      )

makeRequest :: APIRequest apiName responseType
            -> IO HTTP.Request
makeRequest (APIRequestGet u pa) = makeRequest' "GET" u (makeSimpleQuery pa)
makeRequest (APIRequestPost u pa) = makeRequest' "POST" u (makeSimpleQuery pa)
makeRequest (APIRequestPostMultipart u param prt) =
    formDataBody body =<< makeRequest' "POST" u []
  where
    body = prt ++ partParam
    partParam = Prelude.map (uncurry partBS . over _1 T.decodeUtf8) (makeSimpleQuery param)

makeRequest' :: HT.Method -- ^ HTTP request method (GET or POST)
             -> String -- ^ API Resource URL
             -> HT.SimpleQuery -- ^ Query
             -> IO HTTP.Request
makeRequest' m url query = do
    req <- HTTP.parseUrl url
    return $ req { HTTP.method = m
                 , HTTP.queryString = HT.renderSimpleQuery False query
                 , HTTP.checkStatus = \_ _ _ -> Nothing
                 }

getResponse :: MonadResource m
            => TWInfo
            -> HTTP.Manager
            -> HTTP.Request
            -> m (Response (C.ResumableSource m ByteString))
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

getValue :: MonadThrow m
         => Response (C.ResumableSource m ByteString)
         -> m (Response Value)
getValue res = do
    value <- responseBody res C.$$+- sinkJSON
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

getValueOrThrow :: (MonadThrow m, FromJSON a)
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

call :: (MonadResource m, FromJSON responseType)
     => TWInfo
     -> HTTP.Manager
     -> APIRequest apiName responseType
     -> m responseType
call = call'

call' :: (MonadResource m, FromJSON value)
      => TWInfo
      -> HTTP.Manager
      -> APIRequest apiName responseType
      -> m value
call' info mgr req = responseBody `fmap` callWithResponse' info mgr req

callWithResponse :: (MonadResource m, FromJSON responseType)
                 => TWInfo
                 -> HTTP.Manager
                 -> APIRequest apiName responseType
                 -> m (Response responseType)
callWithResponse = callWithResponse'

callWithResponse' :: (MonadResource m, FromJSON value)
                  => TWInfo
                  -> HTTP.Manager
                  -> APIRequest apiName responseType
                  -> m (Response value)
callWithResponse' info mgr req = do
    res <- getResponse info mgr =<< liftIO (makeRequest req)
    getValueOrThrow res

sourceWithMaxId :: ( MonadResource m
                   , FromJSON responseType
                   , AsStatus responseType
                   , HasMaxIdParam (APIRequest apiName [responseType])
                   )
                => TWInfo
                -> HTTP.Manager
                -> APIRequest apiName [responseType]
                -> C.Source m responseType
sourceWithMaxId info mgr = loop
  where
    loop req = do
        res <- lift $ call info mgr req
        case getMinId res of
            Just mid -> do
                CL.sourceList res
                loop $ req & maxId ?~ mid - 1
            Nothing -> CL.sourceList res
    getMinId = minimumOf (traverse . status_id)

sourceWithMaxId' :: ( MonadResource m
                    , HasMaxIdParam (APIRequest apiName [responseType])
                    )
                 => TWInfo
                 -> HTTP.Manager
                 -> APIRequest apiName [responseType]
                 -> C.Source m Value
sourceWithMaxId' info mgr = loop
  where
    loop req = do
        res <- lift $ call' info mgr req
        case getMinId res of
            Just mid -> do
                CL.sourceList res
                loop $ req & maxId ?~ mid - 1
            Nothing -> CL.sourceList res
    getMinId = minimumOf (traverse . key "id" . _Integer)

sourceWithCursor :: ( MonadResource m
                    , FromJSON responseType
                    , CursorKey ck
                    , HasCursorParam (APIRequest apiName (WithCursor ck responseType))
                    )
                 => TWInfo
                 -> HTTP.Manager
                 -> APIRequest apiName (WithCursor ck responseType)
                 -> C.Source m responseType
sourceWithCursor info mgr req = loop (-1)
  where
    loop 0 = CL.sourceNull
    loop cur = do
        res <- lift $ call info mgr $ req & cursor ?~ cur
        CL.sourceList $ contents res
        loop $ nextCursor res

sourceWithCursor' :: ( MonadResource m
                     , FromJSON responseType
                     , CursorKey ck
                     , HasCursorParam (APIRequest apiName (WithCursor ck responseType))
                     )
                  => TWInfo
                  -> HTTP.Manager
                  -> APIRequest apiName (WithCursor ck responseType)
                  -> C.Source m Value
sourceWithCursor' info mgr req = loop (-1)
  where
    relax :: FromJSON value
          => APIRequest apiName (WithCursor ck responseType)
          -> APIRequest apiName (WithCursor ck value)
    relax = unsafeCoerce
    loop 0 = CL.sourceNull
    loop cur = do
        res <- lift $ call info mgr $ relax $ req & cursor ?~ cur
        CL.sourceList $ contents res
        loop $ nextCursor res

sinkJSON :: ( MonadThrow m
            ) => C.Consumer ByteString m Value
sinkJSON = CA.sinkParser json

sinkFromJSON :: ( FromJSON a
                , MonadThrow m
                ) => C.Consumer ByteString m a
sinkFromJSON = do
    v <- sinkJSON
    case fromJSON v of
        Error err -> monadThrow $ FromJSONError err
        Success r -> return r

showBS :: Show a => a -> ByteString
showBS = S8.pack . show
