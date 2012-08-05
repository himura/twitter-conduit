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

mkUserParam :: UserParam -> HT.SimpleQuery
mkUserParam (UserIdParam uid) =  [("user_id", showBS uid)]
mkUserParam (ScreenNameParam sn) = [("screen_name", B8.pack sn)]

mkListParam :: ListParam -> HT.SimpleQuery
mkListParam (ListIdParam lid) =  [("list_id", showBS lid)]
mkListParam (ListNameParam listname) =
  [("slug", B8.pack lstName),
   ("owner_screen_name", B8.pack screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln

