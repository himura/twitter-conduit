{-# LANGUAGE RecordWildCards #-}
module Web.Twitter.Enumerator.Api
       ( api
       , endpoint
       ) where

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Monad

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT

import Data.ByteString (ByteString)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import System.IO

endpoint :: String
endpoint = "https://api.twitter.com/1/"

api :: ByteString -- ^ HTTP request method (GET or POST)
    -> String -- ^ API Resource URL
    -> HT.Ascii -- ^ Query
    -> C.ResourceT TW (C.BufferedSource IO ByteString)
api m url query = do
  req <- lift $ apiRequest m url query
  mgr <- lift getManager
  transResourceT lift $ responseBody <$> http req mgr

apiRequest :: ByteString -> String -> HT.Ascii -> TW (Request IO)
apiRequest m uri query = do
  p <- getProxy
  req <- liftIO $ parseUrl uri >>= \r ->
    return $ r { method = m, queryString = query, proxy = p }
  signOAuthTW req
