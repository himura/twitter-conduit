{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#else
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

module Web.Twitter.Conduit.Api
       ( api
       , apiGet
       , apiGet'
       , apiPost
       , apiPost'
       , apiCursor
       , apiCursor'
       , apiWithPages
       , apiWithPages'
       , AuthHandler
       , TwitterBaseM
       , endpoint
       ) where

import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Utils

import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Monoid
import Control.Applicative
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)

#if MIN_VERSION_http_conduit(2, 0, 0)
type AuthHandler m = Request -> TW m Request
#else
type AuthHandler m = Request (TW m) -> TW m (Request (TW m))
#end

#if __GLASGOW_HASKELL__ >= 704
type TwitterBaseM m = (C.MonadResource m, MonadBaseControl IO m)
#else
class (C.MonadResource m, MonadBaseControl IO m) => TwitterBaseM m
instance (C.MonadResource m, MonadBaseControl IO m) => TwitterBaseM m
#endif

api :: (C.MonadResource m, MonadBaseControl IO m)
    => AuthHandler m
    -> HT.Method -- ^ HTTP request method (GET or POST)
    -> String -- ^ API Resource URL
    -> HT.SimpleQuery -- ^ Query
    -> TW m (C.ResumableSource (TW m) ByteString)
api hndl m url query = do
  p <- getProxy
  req  <- liftIO $ parseUrl url
  req' <- hndl req { method = m, queryString = HT.renderSimpleQuery False query, proxy = p }
  mgr  <- getManager
  responseBody <$> http req' mgr

endpoint :: String
endpoint = "https://api.twitter.com/1.1/"

apiGet :: (TwitterBaseM m, A.FromJSON a)
       => AuthHandler m
       -> String -- ^ API Resource URL
       -> HT.SimpleQuery -- ^ Query
       -> TW m a
apiGet hndl u = apiGet' hndl fu
  where fu = endpoint ++ u

apiPost :: (TwitterBaseM m, A.FromJSON a)
        => AuthHandler m
        -> String -- ^ API Resource URL
        -> HT.SimpleQuery -- ^ Query
        -> TW m a
apiPost hndl u = apiPost' hndl fu
  where fu = endpoint ++ u

apiGet' :: (TwitterBaseM m, A.FromJSON a)
        => AuthHandler m
        -> String -- ^ API Resource URL
        -> HT.SimpleQuery -- ^ Query
        -> TW m a
apiGet' hndl url query = do
  src <- api hndl "GET" url query
  src C.$$+- sinkFromJSON

apiPost' :: (TwitterBaseM m, A.FromJSON a)
         => AuthHandler m
         -> String -- ^ API Resource URL
         -> HT.SimpleQuery -- ^ Query
         -> TW m a
apiPost' hndl url query = do
  src <- api hndl "POST" url query
  src C.$$+- sinkFromJSON

apiCursor :: (TwitterBaseM m, A.FromJSON a)
          => AuthHandler m
          -> String -- ^ API Resource URL
          -> HT.SimpleQuery -- ^ Query
          -> T.Text
          -> C.Source (TW m) a
apiCursor hndl u = apiCursor' hndl fu
  where fu = endpoint ++ u

apiCursor' :: (TwitterBaseM m, A.FromJSON a)
           => AuthHandler m
           -> String -- ^ API Resource URL
           -> HT.SimpleQuery -- ^ Query
           -> T.Text
           -> C.Source (TW m) a
apiCursor' hndl url query cursorKey = loop (-1 :: Int)
  where
    loop 0 = CL.sourceNull
    loop cursor = do
          let query' = ("cursor", showBS cursor) `insertQuery` query
          j <- lift $ do
            src <- api hndl "GET" url query'
            src C.$$+- sinkJSON
          case A.parseMaybe p j of
            Nothing -> CL.sourceNull
            Just (res, nextCursor) -> do
              CL.sourceList res
              loop nextCursor

    p (A.Object v) = (,) <$> v A..: cursorKey <*> v A..: "next_cursor"
    p _ = mempty

apiWithPages :: (TwitterBaseM m, A.FromJSON a)
             => AuthHandler m
             -> String -- ^ API Resource URL
             -> HT.SimpleQuery -- ^ Query
             -> C.Source (TW m) a
apiWithPages hndl u = apiWithPages' hndl fu
  where fu = endpoint ++ u

apiWithPages' :: (TwitterBaseM m, A.FromJSON a)
              => AuthHandler m
              -> String -- ^ API Resource URL
              -> HT.SimpleQuery -- ^ Query
              -> C.Source (TW m) a
apiWithPages' hndl url query = loop (1 :: Int)
  where loop page = do
          let query' = ("page", showBS page) `insertQuery` query
          rs <- lift $ do
            src <- api hndl "GET" url query'
            src C.$$+- sinkFromJSON
          if null rs
            then CL.sourceNull
            else CL.sourceList rs >> loop (page+1)
