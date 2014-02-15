{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Conduit.Param
       ( UserParam(..)
       , UserListParam(..)
       , ListParam(..)
       , mkUserParam
       , mkUserListParam
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
data UserListParam = UserIdListParam [UserId] | ScreenNameListParam [String]
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

-- | converts 'UserListParam' to 'HT.SimpleQuery'.
--
-- >>> mkUserParam $ UserIdListParam [123456]
-- [("user_id","123456")]
-- >>> mkUserParam $ UserIdListParam [123456, 654321]
-- [("user_id","123456"),("user_id","654321")]
-- >>> mkUserParam $ ScreenNameListParam ["thimura", "NikaidouShinku"]
-- [("screen_name","thimura"),("screen_name","NikaidouShinku")]
mkUserListParam :: UserListParam -> HT.SimpleQuery
mkUserListParam (UserIdListParam uids) =  [("user_id", B8.intercalate "," . map showBS $ uids)]
mkUserListParam (ScreenNameListParam sns) = [("screen_name", B8.intercalate "," . map B8.pack $ sns)]

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

