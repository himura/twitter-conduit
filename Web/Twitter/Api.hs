{-# LANGUAGE RecordWildCards #-}
module Web.Twitter.Api
       ( api
       , endpoint
       ) where

import Web.Twitter.Types
import Web.Twitter.Monad

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT
import System.IO

endpoint :: String
endpoint = "https://api.twitter.com/1/"

api :: ByteString -- ^ HTTP request method (GET or POST)
    -> String -- ^ API Resource URL
    -> HT.Query -- ^ Query
    -> C.ResourceT TW (C.Source TW ByteString)
api m url query = do
  (req, mgr) <- lift $ do
    p    <- getProxy
    req  <- parseUrl url
    req' <- signOAuthTW $ req { method = m, queryString = HT.renderQuery False query, proxy = p }
    mgr  <- getManager
    return (req', mgr)
  responseBody <$> http req mgr
