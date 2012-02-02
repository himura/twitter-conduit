{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Web.Twitter.Api (
  api,
  apiGet,
  apiCursor,
  apiWithPages,
  ) where

import Web.Twitter.Monad
import Web.Twitter.Utils

import Control.Applicative
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Monoid
import qualified Data.Text as T
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT

endpoint :: String
endpoint = "https://api.twitter.com/1/"

join :: C.ResourceIO m => C.ResourceT m (C.Source m a) -> C.Source m a
join m = C.Source (C.Open <$> n <*> pure []) (return ()) C.$= CL.concatMap id
  where
    n = fmap (\a -> [a]) <$> m

api :: ByteString -- ^ HTTP request method (GET or POST)
    -> String     -- ^ API Resource URL
    -> HT.Query   -- ^ Query
    -> C.Source TW ByteString
api m url query = join $ do
  (req, mgr) <- lift $ do
    p    <- getProxy
    req  <- parseUrl url
    req' <- signOAuthTW $ req { method = m, queryString = HT.renderQuery False query, proxy = p }
    mgr  <- getManager
    return (req', mgr)  
  responseBody <$> http req mgr

apiGet :: FromJSON a => String -> HT.Query -> TW a
apiGet url query =
  C.runResourceT $ api "GET" url query C.$$ sinkFromJSON

apiCursor :: FromJSON a
             => String
             -> HT.Query
             -> T.Text
             -> C.Source TW a
apiCursor url query cursorKey = join $ go (-1 :: Int) where
  go cursor = do
    let query' = ("cursor", Just $ showBS cursor) `insertQuery` query
    j <- api "GET" (endpoint ++ url) query' C.$$ sinkJSON
    case parseMaybe p j of
      Nothing ->
        return CL.sourceNull
      Just (res, 0) ->
        return $ CL.sourceList res
      Just (res, nextCursor) ->
        mappend (CL.sourceList res) <$> go nextCursor

  p (Object v) = (,) <$> v .: cursorKey <*> v .: "next_cursor"
  p _ = mempty


apiWithPages :: (FromJSON a, Show a) => String -> HT.Query -> C.Source TW a
apiWithPages url query = C.sourceState (1 :: Int) pull C.$= CL.concatMap id where
  pull page = do
    let query' = ("page", Just $ showBS page) `insertQuery` query
    rs <- api "GET" (endpoint ++ url) query' C.$$ sinkFromJSON
    case rs of
      [] -> return C.StateClosed
      _ -> return $ C.StateOpen (page + 1) rs
