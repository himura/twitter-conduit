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
join m = C.Source (C.Open <$> m <*> undefined) (return ())

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
  C.runResourceT $ do
    api "GET" url query C.$$ sinkFromJSON

apiCursor :: FromJSON a
             => String
             -> HT.Query
             -> T.Text
             -> C.ResourceT TW (C.Source TW a)
apiCursor = undefined
  {-
apiCursor url query cursorKey = go (-1 :: Int) where
  go cursor = do
    let query' = ("cursor", Just $ showBS cursor) `insertQuery` query
    res <- api "GET" (endpoint ++ url) query'
    j <- res C.$$ sinkJSON
    case parseMaybe p j of
      Nothing ->
        return CL.sourceNull
      Just (res, 0) ->
        return $ CL.sourceList res
      Just (res, nextCursor) ->
        mappend (CL.sourceList res) <$> go nextCursor

  p (Object v) = (,) <$> v .: cursorKey <*> v .: "next_cursor"
  p _ = mempty
-}
  
apiWithPages :: (FromJSON a, Show a) => String -> HT.Query -> C.ResourceT TW (C.Source TW a)
apiWithPages = undefined
{-
apiWithPages uri query initPage =
  checkContinue1 go initPage
  where
    go loop page k = do
      let query' = insertQuery "page" (toMaybeByteString page) query
      res <- lift $ run_ $ api "GET" uri query' (handleParseError (enumJSON =$ iterPageC))
      case res of
        Just [] -> k EOF
        Just xs -> k (Chunks xs) >>== loop (page + 1)
        Nothing -> k EOF

iterPageC :: (C.Resource m, Monad m, FromJSON a) => C.Sink Value m (Maybe [a])
iterPageC = do
  ret <- CL.head
  case ret of
    Just v -> return . fromJSON' $ v
    Nothing -> return Nothing
-}
