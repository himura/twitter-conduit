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

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Monoid
import Control.Applicative
import Control.Failure
import Control.Monad.Trans.Control

type AuthHandler cred m = Request (TW cred m) -> TW cred m (Request (TW cred m))

#if __GLASGOW_HASKELL__ >= 704
type TwitterBaseM m = (C.MonadResource m, MonadBaseControl IO m, Failure HttpException m)
#else
class (C.MonadResource m, MonadBaseControl IO m, Failure HttpException m) => TwitterBaseM m
instance (C.MonadResource m, MonadBaseControl IO m, Failure HttpException m) => TwitterBaseM m
#endif

api :: TwitterBaseM m
    => AuthHandler cred m
    -> HT.Method -- ^ HTTP request method (GET or POST)
    -> String -- ^ API Resource URL
    -> HT.Query -- ^ Query
    -> C.Source (TW cred m) ByteString
api hndl m url query = flip C.PipeM (return ()) $ do
  p <- getProxy
  req  <- parseUrl url
  req' <- hndl req { method = m, queryString = HT.renderQuery False query, proxy = p }
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
       -> HT.Query -- ^ Query
       -> TW cred m a
apiGet hndl u = apiGet' hndl fu
  where fu = endpoint ++ u

apiGet' :: (TwitterBaseM m, A.FromJSON a)
        => AuthHandler cred m
        -> String -- ^ API Resource URL
        -> HT.Query -- ^ Query
        -> TW cred m a
apiGet' hndl url query =
  api hndl "GET" url query C.$$ sinkFromJSON

apiCursor :: (TwitterBaseM m, A.FromJSON a)
          => AuthHandler cred m
          -> String -- ^ API Resource URL
          -> HT.Query -- ^ Query
          -> T.Text
          -> C.Source (TW cred m) a
apiCursor hndl u = apiCursor' hndl fu
  where fu = endpoint ++ u

apiCursor' :: (TwitterBaseM m, A.FromJSON a)
           => AuthHandler cred m
           -> String -- ^ API Resource URL
           -> HT.Query -- ^ Query
           -> T.Text
           -> C.Source (TW cred m) a
apiCursor' hndl url query cursorKey = C.PipeM (go (-1 :: Int)) (return ())
  where
    go cursor = do
      let query' = ("cursor", Just $ showBS cursor) `insertQuery` query
      j <- api hndl "GET" url query' C.$$ sinkJSON
      case A.parseMaybe p j of
        Nothing ->
          return CL.sourceNull
        Just (res, 0) ->
          return $ CL.sourceList res
        Just (res, nextCursor) ->
          mappend (CL.sourceList res) <$> go nextCursor

    p (A.Object v) = (,) <$> v A..: cursorKey <*> v A..: "next_cursor"
    p _ = mempty

apiWithPages :: (TwitterBaseM m, A.FromJSON a)
             => AuthHandler cred m
             -> String -- ^ API Resource URL
             -> HT.Query -- ^ Query
             -> C.Source (TW cred m) a
apiWithPages hndl u = apiWithPages' hndl fu
  where fu = endpoint ++ u

apiWithPages' :: (TwitterBaseM m, A.FromJSON a)
              => AuthHandler cred m
              -> String -- ^ API Resource URL
              -> HT.Query -- ^ Query
              -> C.Source (TW cred m) a
apiWithPages' hndl url query = C.sourceState (1 :: Int) pull C.$= CL.concatMap id where
  pull page = do
    let query' = ("page", Just $ showBS page) `insertQuery` query
    rs <- api hndl "GET" url query' C.$$ sinkFromJSON
    case rs of
      [] -> return C.StateClosed
      _ -> return $ C.StateOpen (page + 1) rs
