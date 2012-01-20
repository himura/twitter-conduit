{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Enumerator.Post where

import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Api

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Enumerator hiding (map, filter, drop, span, iterate)

update :: ByteString -> Iteratee ByteString IO a -> Iteratee ByteString TW a
update tw iter = api (B8.pack "POST") (endpoint ++ "statuses/update.json") [(B8.pack "status",Just tw)] iter
