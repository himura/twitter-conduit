{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Api
       ( api
       , endpoint
       , endpointSearch
       , UserParam(..)
       , ListParam(..)
       , mkUserParam
       , mkListParam
       ) where

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Utils

import Network.HTTP.Enumerator
import qualified Network.HTTP.Types as HT
import Data.Enumerator (Iteratee, throwError, liftTrans)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Control.Monad.Trans.Class
import Control.Monad.IO.Class (MonadIO (liftIO))

endpoint :: String
endpoint = "https://api.twitter.com/1/"

endpointSearch :: String
endpointSearch = "http://search.twitter.com/"

api :: Bool -- ^ OAuth required?
    -> ByteString -- ^ HTTP request method (GET or POST)
    -> String -- ^ API Resource URL
    -> HT.Query -- ^ Query
    -> Iteratee ByteString IO a
    -> Iteratee ByteString TW a
api toOAuth m url query iter = do
  req <- lift $ apiRequest toOAuth m url query
  httpMgr req (handleError iter)
  where
    handleError iter' st@(HT.Status sc _) _ =
      if 200 <= sc && sc < 300
      then iter'
      else throwError $ HTTPStatusCodeException st

httpMgr :: Request IO
        -> (HT.Status
            -> HT.ResponseHeaders
            -> Iteratee ByteString IO a)
        -> Iteratee ByteString TW a
httpMgr req iterf = do
  mgr <- lift getManager
  liftTrans $ http req iterf mgr

apiRequest :: Bool -> ByteString -> String -> HT.Query -> TW (Request IO)
apiRequest toOAuth m uri query = do
  p <- getProxy
  req <- liftIO $ parseUrl uri >>= \r ->
    return $ r { method = m, queryString = query, proxy = p }
  if toOAuth then signOAuthTW req
             else return req

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
