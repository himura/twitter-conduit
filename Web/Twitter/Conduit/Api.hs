{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Twitter.Conduit.Api
       ( api
       , apiCursor
       , apiWithPages
       , authRequired
       , authSupported
       , noAuth
       , endpoint
       , endpointSearch
       , UserParam(..)
       , ListParam(..)
       , mkUserParam
       , mkListParam
       ) where

import Web.Twitter.Conduit.Types
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
import qualified Data.ByteString.Char8 as B8
import Data.Monoid
import Control.Applicative
import Control.Failure
import Control.Monad.Trans.Control

endpoint :: String
endpoint = "https://api.twitter.com/1/"

endpointSearch :: String
endpointSearch = "http://search.twitter.com/"

type AuthHandler cred m = Request (TW cred m) -> TW cred m (Request (TW cred m))

api :: ( C.MonadResource m
       , MonadBaseControl IO m
       , Failure HttpException m
       )
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

apiAuthRequired :: ( C.MonadResource m
                   , MonadBaseControl IO m
                   , Failure HttpException m
                   )
                => HT.Method -- ^ HTTP request method (GET or POST)
                -> String -- ^ API Resource URL
                -> HT.Query -- ^ Query
                -> C.Source (TW WithToken m) ByteString
apiAuthRequired = api signOAuthTW


authSupported :: C.MonadUnsafeIO m => AuthHandler cred m
authSupported = signOAuthIfExistTW

apiAuthSupported :: ( C.MonadResource m
                    , MonadBaseControl IO m
                    , Failure HttpException m
                    )
                 => HT.Method -- ^ HTTP request method (GET or POST)
                 -> String -- ^ API Resource URL
                 -> HT.Query -- ^ Query
                 -> C.Source (TW cred m) ByteString
apiAuthSupported = api signOAuthIfExistTW

noAuth :: Monad m => AuthHandler NoToken m
noAuth = return

apiNoAuth :: ( C.MonadResource m
             , MonadBaseControl IO m
             , Failure HttpException m
             )
             => HT.Method -- ^ HTTP request method (GET or POST)
             -> String -- ^ API Resource URL
             -> HT.Query -- ^ Query
             -> C.Source (TW NoToken m) ByteString
apiNoAuth = api return

apiCursor :: ( C.MonadResource m
             , MonadBaseControl IO m
             , Failure HttpException m
             , A.FromJSON a
             )
          => AuthHandler cred m
          -> String -- ^ API Resource URL
          -> HT.Query -- ^ Query
          -> T.Text
          -> C.Source (TW cred m) a
apiCursor hndl url query cursorKey = C.PipeM (go (-1 :: Int)) (return ())
  where
    go cursor = do
      let query' = ("cursor", Just $ showBS cursor) `insertQuery` query
      j <- api hndl "GET" (endpoint ++ url) query' C.$$ sinkJSON
      case A.parseMaybe p j of
        Nothing ->
          return CL.sourceNull
        Just (res, 0) ->
          return $ CL.sourceList res
        Just (res, nextCursor) ->
          mappend (CL.sourceList res) <$> go nextCursor

    p (A.Object v) = (,) <$> v A..: cursorKey <*> v A..: "next_cursor"
    p _ = mempty

apiWithPages :: ( C.MonadResource m
                , MonadBaseControl IO m
                , Failure HttpException m
                , A.FromJSON a
                )
             => AuthHandler cred m
             -> String -- ^ API Resource URL
             -> HT.Query -- ^ Query
             -> C.Source (TW cred m) a
apiWithPages hndl url query = C.sourceState (1 :: Int) pull C.$= CL.concatMap id where
  pull page = do
    let query' = ("page", Just $ showBS page) `insertQuery` query
    rs <- api hndl "GET" (endpoint ++ url) query' C.$$ sinkFromJSON
    case rs of
      [] -> return C.StateClosed
      _ -> return $ C.StateOpen (page + 1) rs

data UserParam = UserIdParam UserId | ScreenNameParam String
               deriving (Show, Eq)
data ListParam = ListIdParam Integer | ListNameParam String
               deriving (Show, Eq)

mkUserParam :: UserParam -> HT.Query
mkUserParam (UserIdParam uid) =  [("user_id", toMaybeByteString uid)]
mkUserParam (ScreenNameParam sn) = [("screen_name", Just . B8.pack $ sn)]

mkListParam :: ListParam -> HT.Query
mkListParam (ListIdParam lid) =  [("list_id", toMaybeByteString lid)]
mkListParam (ListNameParam listname) =
  [("slug", Just . B8.pack $ lstName),
   ("owner_screen_name", Just . B8.pack $ screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln
