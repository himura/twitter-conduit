{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Stream (
  -- * StreamingAPI
  userstream,
  statusesFilter,
  ) where

import Web.Twitter.Api
import Web.Twitter.Monad
import Web.Twitter.Types
import Web.Twitter.Utils

import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Network.HTTP.Types as HT

streamingConduit :: ResourceThrow m => C.Conduit ByteString m StreamingAPI
streamingConduit = conduitParser parseFromJSON

userstream :: C.ResourceT TW (C.Source TW StreamingAPI)
userstream = do
  src <- api "GET" "https://userstream.twitter.com/2/user.json" []
  return $ src C.$= streamingConduit

statusesFilter :: HT.Query -> C.ResourceT TW (C.Source TW StreamingAPI)
statusesFilter query = do
  src <- api "GET" "https://stream.twitter.com/1/statuses/filter.json" query
  return $ src C.$= streamingConduit
