{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#else
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

module Web.Twitter.Conduit.Base
       ( api
       , apiRequest
       , apiGet
       , apiGet'
       , apiPost
       , apiPost'
       , call
       , apiCursor
       , apiCursor'
       , sourceWithMaxId
       , apiWithPages
       , apiWithPages'
       , TwitterBaseM
       , endpoint
       , makeRequest
       ) where

import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Utils
import Web.Twitter.Conduit.Request

import Network.HTTP.Conduit
import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Types as HT
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Text.Shakespeare.Text
import Control.Monad.Logger
import Control.Arrow (first)
import Control.Lens

#if __GLASGOW_HASKELL__ >= 704
type TwitterBaseM m = ( C.MonadResource m
                      , MonadLogger m
                      )
#else
class (C.MonadResource m, MonadLogger m) => TwitterBaseM m
instance (C.MonadResource m, MonadLoger m) => TwitterBaseM m
#endif

makeRequest :: MonadIO m
            => HT.Method -- ^ HTTP request method (GET or POST)
            -> String -- ^ API Resource URL
            -> HT.SimpleQuery -- ^ Query
            -> TW m Request
makeRequest m url query = do
    p <- getProxy
    req <- liftIO $ parseUrl url
    return $ req { method = m
                 , queryString = HT.renderSimpleQuery False query
                 , proxy = p }

api :: TwitterBaseM m
    => HT.Method -- ^ HTTP request method (GET or POST)
    -> String -- ^ API Resource URL
    -> HT.SimpleQuery -- ^ Query
    -> TW m (C.ResumableSource (TW m) ByteString)
api m url query =
    apiRequest =<< makeRequest m url query

apiRequest :: TwitterBaseM m
           => Request
           -> TW m (C.ResumableSource (TW m) ByteString)
apiRequest req = do
    signedReq <- signOAuthTW req
    $(logDebug) [st|Signed Request: #{show signedReq}|]
    mgr <- getManager
    res <- http signedReq mgr
    $(logDebug) [st|Response Status: #{show $ responseStatus res}|]
    $(logDebug) [st|Response Header: #{show $ responseHeaders res}|]
    return $ responseBody res

endpoint :: String
endpoint = "https://api.twitter.com/1.1/"

apiGet :: (TwitterBaseM m, A.FromJSON a)
       => String -- ^ API Resource URL
       -> HT.SimpleQuery -- ^ Query
       -> TW m a
apiGet u = apiGet' fu
  where fu = endpoint ++ u

apiPost :: (TwitterBaseM m, A.FromJSON a)
        => String -- ^ API Resource URL
        -> HT.SimpleQuery -- ^ Query
        -> TW m a
apiPost u = apiPost' fu
  where fu = endpoint ++ u

apiGet' :: (TwitterBaseM m, A.FromJSON a)
        => String -- ^ API Resource URL
        -> HT.SimpleQuery -- ^ Query
        -> TW m a
apiGet' url query = do
    src <- api "GET" url query
    src C.$$+- sinkFromJSON

apiPost' :: (TwitterBaseM m, A.FromJSON a)
         => String -- ^ API Resource URL
         -> HT.SimpleQuery -- ^ Query
         -> TW m a
apiPost' url query = do
    src <- api "POST" url query
    src C.$$+- sinkFromJSON

call :: (TwitterBaseM m, A.FromJSON responseType)
     => APIRequest apiName responseType
     -> TW m (APIResponse responseType)
call = fmap APIResponse . call'

call' :: (TwitterBaseM m, A.FromJSON responseType)
      => APIRequest apiName responseType
      -> TW m A.Value
call' (APIRequestGet u pa) = apiGet' u pa
call' (APIRequestPost u pa) = apiPost' u pa
call' (APIRequestPostMultipart u param prt) = do
    req <- formDataBody body =<< makeRequest "POST" u []
    src <- apiRequest req
    src C.$$+- sinkFromJSON
  where
    body = prt ++ partParam
    partParam = map (uncurry partBS . first T.decodeUtf8) param

apiCursor :: (TwitterBaseM m, A.FromJSON a)
          => String -- ^ API Resource URL
          -> HT.SimpleQuery -- ^ Query
          -> T.Text
          -> C.Source (TW m) a
apiCursor u = apiCursor' fu
  where fu = endpoint ++ u

apiCursor' :: (TwitterBaseM m, A.FromJSON a)
           => String -- ^ API Resource URL
           -> HT.SimpleQuery -- ^ Query
           -> T.Text
           -> C.Source (TW m) a
apiCursor' url query cursorKey = loop (-1 :: Int)
  where
    loop 0 = CL.sourceNull
    loop cursor = do
        let query' = ("cursor", showBS cursor) `insertQuery` query
        j <- lift $ do
            src <- api "GET" url query'
            src C.$$+- sinkJSON
        case A.parseMaybe p j of
            Nothing -> CL.sourceNull
            Just (res, nextCursor) -> do
                CL.sourceList res
                loop nextCursor

    p (A.Object v) = (,) <$> v A..: cursorKey <*> v A..: "next_cursor"
    p _ = mempty

sourceWithMaxId :: ( TwitterBaseM m
                   , A.FromJSON responseType
                   , HasMaxIdParam (APIRequest apiName [responseType])
                   )
                => APIRequest apiName [responseType]
                -> C.Source (TW m) responseType
sourceWithMaxId = loop
  where
    loop req = do
        res <- lift $ call req
        case (getMinId res, res ^. parsed) of
            (Just mid, Just list) -> do
                CL.sourceList list
                loop $ req & maxId ?~ mid - 1
            (_, Just list) -> CL.sourceList list
            (_, _) -> CL.sourceList []
    getMinId = minimumOf (_Array . traverse . key "id" . _Integer)

apiWithPages :: (TwitterBaseM m, A.FromJSON a)
             => String -- ^ API Resource URL
             -> HT.SimpleQuery -- ^ Query
             -> C.Source (TW m) a
apiWithPages u = apiWithPages' fu
  where fu = endpoint ++ u

apiWithPages' :: (TwitterBaseM m, A.FromJSON a)
              => String -- ^ API Resource URL
              -> HT.SimpleQuery -- ^ Query
              -> C.Source (TW m) a
apiWithPages' url query = loop (1 :: Int)
  where
    loop page = do
        let query' = ("page", showBS page) `insertQuery` query
        rs <- lift $ do
            src <- api "GET" url query'
            src C.$$+- sinkFromJSON
        if null rs
            then CL.sourceNull
            else CL.sourceList rs >> loop (page+1)
