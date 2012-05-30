{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Conduit.Param
       ( UserParam(..)
       , ListParam(..)
       , mkUserParam
       , mkListParam
       ) where

import Web.Twitter.Conduit.Types
import Web.Twitter.Conduit.Utils

import qualified Network.HTTP.Types as HT
import qualified Data.ByteString.Char8 as B8

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

