{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Conduit.Param
       ( UserParam(..)
       , ListParam(..)
       , mkUserParam
       , mkListParam
       ) where

import Web.Twitter.Types
import Web.Twitter.Conduit.Utils

import qualified Network.HTTP.Types as HT
import qualified Data.ByteString.Char8 as B8

-- $setup
-- >>> :set -XOverloadedStrings

data UserParam = UserIdParam UserId | ScreenNameParam String
               deriving (Show, Eq)
data ListParam = ListIdParam Integer | ListNameParam String
               deriving (Show, Eq)

-- | converts 'UserParam' to 'HT.SimpleQuery'.
--
-- >>> mkUserParam $ UserIdParam 123456
-- [("user_id","123456")]
-- >>> mkUserParam $ ScreenNameParam "thimura"
-- [("screen_name","thimura")]
mkUserParam :: UserParam -> HT.SimpleQuery
mkUserParam (UserIdParam uid) =  [("user_id", showBS uid)]
mkUserParam (ScreenNameParam sn) = [("screen_name", B8.pack sn)]

-- | converts 'ListParam' to 'HT.SimpleQuery'.
--
-- >>> mkListParam $ ListIdParam 123123
-- [("list_id","123123")]
-- >>> mkListParam $ ListNameParam "thimura/haskell"
-- [("slug","haskell"),("owner_screen_name","thimura")]
mkListParam :: ListParam -> HT.SimpleQuery
mkListParam (ListIdParam lid) =  [("list_id", showBS lid)]
mkListParam (ListNameParam listname) =
  [("slug", B8.pack lstName),
   ("owner_screen_name", B8.pack screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln

