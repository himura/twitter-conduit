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

userstream :: C.Source TW StreamingAPI
userstream =
  api "GET" "https://userstream.twitter.com/2/user.json" []
    C.$= streamingConduit

statusesFilter :: HT.Query -> C.Source TW StreamingAPI
statusesFilter query =
  api "GET" "https://stream.twitter.com/1/statuses/filter.json" query
    C.$= streamingConduit
