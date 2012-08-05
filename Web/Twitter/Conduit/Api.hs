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
       , authRequired
       , authSupported
       , noAuth
       , TwitterBaseM
       , endpoint
       ) where

import Web.Twitter.Conduit.Monad
import Web.Twitter.Conduit.Utils

import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util as CU

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Monoid
import Control.Applicative
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

type AuthHandler cred m = Request (TW cred m) -> TW cred m (Request (TW cred m))

#if __GLASGOW_HASKELL__ >= 704
type TwitterBaseM m = (C.MonadResource m, MonadBaseControl IO m)
#else
class (C.MonadResource m, MonadBaseControl IO m) => TwitterBaseM m
instance (C.MonadResource m, MonadBaseControl IO m) => TwitterBaseM m
#endif

api :: (C.MonadResource m, MonadBaseControl IO m)
    => AuthHandler cred m
    -> HT.Method -- ^ HTTP request method (GET or POST)
    -> String -- ^ API Resource URL
    -> HT.SimpleQuery -- ^ Query
    -> TW cred m (C.ResumableSource (TW cred m) ByteString)
api hndl m url query = do
  p <- getProxy
  req  <- liftIO $ parseUrl url
  req' <- hndl req { method = m, queryString = HT.renderSimpleQuery False query, proxy = p }
  mgr  <- getManager
  responseBody <$> http req' mgr

authRequired :: C.MonadUnsafeIO m => AuthHandler WithToken m
authRequired = signOAuthTW

authSupported :: C.MonadUnsafeIO m => AuthHandler cred m
authSupported = signOAuthIfExistTW

noAuth :: Monad m => AuthHandler cred m
noAuth = return

endpoint :: String
endpoint = "https://api.twitter.com/1/"

apiGet :: (TwitterBaseM m, A.FromJSON a)
       => AuthHandler cred m
       -> String -- ^ API Resource URL
       -> HT.SimpleQuery -- ^ Query
       -> TW cred m a
apiGet hndl u = apiGet' hndl fu
  where fu = endpoint ++ u

apiPost :: (TwitterBaseM m, A.FromJSON a)
        => AuthHandler cred m
        -> String -- ^ API Resource URL
        -> HT.SimpleQuery -- ^ Query
        -> TW cred m a
apiPost hndl u = apiPost' hndl fu
  where fu = endpoint ++ u

apiGet' :: (TwitterBaseM m, A.FromJSON a)
        => AuthHandler cred m
        -> String -- ^ API Resource URL
        -> HT.SimpleQuery -- ^ Query
        -> TW cred m a
apiGet' hndl url query = do
  src <- api hndl "GET" url query
  src C.$$+- sinkFromJSON

apiPost' :: (TwitterBaseM m, A.FromJSON a)
         => AuthHandler cred m
         -> String -- ^ API Resource URL
         -> HT.SimpleQuery -- ^ Query
         -> TW cred m a
apiPost' hndl url query = do
  src <- api hndl "POST" url query
  src C.$$+- sinkFromJSON

apiCursor :: (TwitterBaseM m, A.FromJSON a)
          => AuthHandler cred m
          -> String -- ^ API Resource URL
          -> HT.SimpleQuery -- ^ Query
          -> T.Text
          -> C.Source (TW cred m) a
apiCursor hndl u = apiCursor' hndl fu
  where fu = endpoint ++ u

apiCursor' :: (TwitterBaseM m, A.FromJSON a)
           => AuthHandler cred m
           -> String -- ^ API Resource URL
           -> HT.SimpleQuery -- ^ Query
           -> T.Text
           -> C.Source (TW cred m) a
apiCursor' hndl url query cursorKey = CU.sourceState (Just (-1 :: Int)) pull C.$= CL.concatMap id
  where
    pull (Just cursor) = do
      let query' = ("cursor", showBS cursor) `insertQuery` query
      src <- api hndl "GET" url query'
      j <- src C.$$+- sinkJSON
      case A.parseMaybe p j of
        Nothing ->
          return CU.StateClosed
        Just (res, 0) ->
          return $ CU.StateOpen Nothing res
        Just (res, nextCursor) -> return $ CU.StateOpen (Just nextCursor) res
    pull Nothing = return CU.StateClosed

    p (A.Object v) = (,) <$> v A..: cursorKey <*> v A..: "next_cursor"
    p _ = mempty

apiWithPages :: (TwitterBaseM m, A.FromJSON a)
             => AuthHandler cred m
             -> String -- ^ API Resource URL
             -> HT.SimpleQuery -- ^ Query
             -> C.Source (TW cred m) a
apiWithPages hndl u = apiWithPages' hndl fu
  where fu = endpoint ++ u

apiWithPages' :: (TwitterBaseM m, A.FromJSON a)
              => AuthHandler cred m
              -> String -- ^ API Resource URL
              -> HT.SimpleQuery -- ^ Query
              -> C.Source (TW cred m) a
apiWithPages' hndl url query = CU.sourceState (1 :: Int) pull C.$= CL.concatMap id where
  pull page = do
    let query' = ("page", showBS page) `insertQuery` query
    src <- api hndl "GET" url query'
    rs <- src C.$$+- sinkFromJSON
    case rs of
      [] -> return CU.StateClosed
      _ -> return $ CU.StateOpen (page + 1) rs
