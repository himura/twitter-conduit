{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Twitter.Conduit.Api
       ( api
       , apiGet
       , apiCursor
       , apiWithPages
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

endpoint :: String
endpoint = "https://api.twitter.com/1/"

endpointSearch :: String
endpointSearch = "http://search.twitter.com/"

api :: RequireAuth -- ^ OAuth required?
    -> ByteString -- ^ HTTP request method (GET or POST)
    -> String -- ^ API Resource URL
    -> HT.Query -- ^ Query
    -> C.Source TW ByteString
api authp m url query = flip C.PipeM (return ()) $ do
  p    <- getProxy
  req  <- parseUrl url
  req' <- signOAuthTW authp $ req { method = m, queryString = HT.renderQuery False query, proxy = p }
  mgr  <- getManager
  responseBody <$> http req' mgr

apiGet :: A.FromJSON a
       => RequireAuth -- ^ OAuth required?
       -> String -- ^ API Resource URL
       -> HT.Query -- ^ Query
       -> TW a
apiGet authp url query =
  api authp "GET" url query C.$$ sinkFromJSON

apiCursor :: A.FromJSON a
          => RequireAuth -- ^ OAuth required?
          -> String -- ^ API Resource URL
          -> HT.Query -- ^ Query
          -> T.Text --
          -> C.Source TW a
apiCursor authp url query cursorKey = flip C.PipeM (return ()) $ go (-1 :: Int) where
  go cursor = do
    let query' = ("cursor", Just $ showBS cursor) `insertQuery` query
    j <- api authp "GET" (endpoint ++ url) query' C.$$ sinkJSON
    case A.parseMaybe p j of
      Nothing ->
        return CL.sourceNull
      Just (res, 0) ->
        return $ CL.sourceList res
      Just (res, nextCursor) ->
        mappend (CL.sourceList res) <$> go nextCursor

  p (A.Object v) = (,) <$> v A..: cursorKey <*> v A..: "next_cursor"
  p _ = mempty


apiWithPages :: A.FromJSON a
             => RequireAuth -- ^ OAuth required?
             -> String -- ^ API Resource URL
             -> HT.Query -- ^ Query
             -> C.Source TW a
apiWithPages authp url query = C.sourceState (1 :: Int) pull C.$= CL.concatMap id where
  pull page = do
    let query' = ("page", Just $ showBS page) `insertQuery` query
    rs <- api authp "GET" (endpoint ++ url) query' C.$$ sinkFromJSON
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
