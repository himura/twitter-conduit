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
import qualified Data.ByteString.Char8 as S8

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
mkUserParam (ScreenNameParam sn) = [("screen_name", S8.pack sn)]

-- | converts 'UserListParam' to 'HT.SimpleQuery'.
--
-- >>> mkUserListParam $ UserIdListParam [123456]
-- [("user_id","123456")]
-- >>> mkUserListParam $ UserIdListParam [123456, 654321]
-- [("user_id","123456,654321")]
-- >>> mkUserListParam $ ScreenNameListParam ["thimura", "NikaidouShinku"]
-- [("screen_name","thimura,NikaidouShinku")]
mkUserListParam :: UserListParam -> HT.SimpleQuery
mkUserListParam (UserIdListParam uids) =  [("user_id", S8.intercalate "," . map showBS $ uids)]
mkUserListParam (ScreenNameListParam sns) = [("screen_name", S8.intercalate "," . map S8.pack $ sns)]

-- | converts 'ListParam' to 'HT.SimpleQuery'.
--
-- >>> mkListParam $ ListIdParam 123123
-- [("list_id","123123")]
-- >>> mkListParam $ ListNameParam "thimura/haskell"
-- [("slug","haskell"),("owner_screen_name","thimura")]
mkListParam :: ListParam -> HT.SimpleQuery
mkListParam (ListIdParam lid) =  [("list_id", showBS lid)]
mkListParam (ListNameParam listname) =
    [("slug", S8.pack lstName),
     ("owner_screen_name", S8.pack screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln

