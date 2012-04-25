{-# LANGUAGE RecordWildCards, OverloadedStrings,FlexibleContexts #-}
module Web.Twitter.Api (
  api,
  apiGet,
  apiCursor,
  apiWithPages,
  ) where

import Web.Twitter.Monad
import Web.Twitter.Utils

import Control.Applicative
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Monoid
import qualified Data.Text as T
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT

endpoint :: String
endpoint = "https://api.twitter.com/1/"



api :: ByteString -- ^ HTTP request method (GET or POST)
    -> String     -- ^ API Resource URL
    -> HT.Query   -- ^ Query
    -> C.Source TW ByteString
api m url query = flip C.PipeM (return ()) $ do
  p    <- getProxy
  req  <- parseUrl url
  req' <- signOAuthTW $ req { method = m, queryString = HT.renderQuery False query, proxy = p }
  mgr  <- getManager
  responseBody <$> http req' mgr

apiGet :: A.FromJSON a => String -> HT.Query -> TW a
apiGet url query =
  api "GET" url query C.$$ sinkFromJSON

apiCursor :: A.FromJSON a
             => String
             -> HT.Query
             -> T.Text
             -> C.Source TW a
apiCursor url query cursorKey = flip C.PipeM (return ()) $ go (-1 :: Int) where
  go cursor = do
    let query' = ("cursor", Just $ showBS cursor) `insertQuery` query
    j <- api "GET" (endpoint ++ url) query' C.$$ sinkJSON
    case A.parseMaybe p j of
      Nothing ->
        return CL.sourceNull
      Just (res, 0) ->
        return $ CL.sourceList res
      Just (res, nextCursor) ->
        mappend (CL.sourceList res) <$> go nextCursor

  p (A.Object v) = (,) <$> v A..: cursorKey <*> v A..: "next_cursor"
  p _ = mempty


apiWithPages :: (A.FromJSON a, Show a) => String -> HT.Query -> C.Source TW a
apiWithPages url query = C.sourceState (1 :: Int) pull C.$= CL.concatMap id where
  pull page = do
    let query' = ("page", Just $ showBS page) `insertQuery` query
    rs <- api "GET" (endpoint ++ url) query' C.$$ sinkFromJSON
    case rs of
      [] -> return C.StateClosed
      _ -> return $ C.StateOpen (page + 1) rs
