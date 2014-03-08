{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Twitter.Conduit.Parameters
       ( Parameters (..)
       , HasSinceIdParam (..)
       , HasCountParam (..)
       , HasMaxIdParam (..)
       , HasPageParam (..)
       , HasCursorParam (..)
       , HasTrimUserParam (..)
       , HasExcludeRepliesParam (..)
       , HasContributorDetailsParam (..)
       , HasIncludeEntitiesParam (..)
       , HasIncludeUserEntitiesParam (..)
       , HasIncludeRtsParam (..)
       , HasIncludeMyRetweetParam (..)
       , HasInReplyToStatusIdParam (..)
       , HasDisplayCoordinatesParam (..)
       , HasPossiblySensitiveParam (..)
       , HasLangParam (..)
       , HasLocaleParam (..)
       , HasUntilParam (..)
       , HasSkipStatusParam (..)
       , HasFollowParam (..)

       , UserParam(..)
       , UserListParam(..)
       , ListParam(..)
       , mkUserParam
       , mkUserListParam
       , mkListParam
       ) where

import Web.Twitter.Conduit.Parameters.Internal
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Types
import Data.Time.Calendar (Day)
import Data.Text.Strict.Lens
import Data.Text (Text)
import Control.Lens

import qualified Network.HTTP.Types as HT
import qualified Data.ByteString.Char8 as S8

data UserParam = UserIdParam UserId | ScreenNameParam String
               deriving (Show, Eq)
data UserListParam = UserIdListParam [UserId] | ScreenNameListParam [String]
                   deriving (Show, Eq)
data ListParam = ListIdParam Integer | ListNameParam String
               deriving (Show, Eq)

defineHasParamClass "count" ''Integer 'readShow
defineHasParamClass "since_id" ''Integer 'readShow
defineHasParamClass "max_id" ''Integer 'readShow
defineHasParamClass "page" ''Integer 'readShow
defineHasParamClass "cursor" ''Integer 'readShow
defineHasParamClass "trim_user" ''Bool 'booleanQuery
defineHasParamClass "exclude_replies" ''Bool 'booleanQuery
defineHasParamClass "contributor_details" ''Bool 'booleanQuery
defineHasParamClass "include_entities" ''Bool 'booleanQuery
defineHasParamClass "include_user_entities" ''Bool 'booleanQuery
defineHasParamClass "include_rts" ''Bool 'booleanQuery
defineHasParamClass "include_my_retweet" ''Bool 'booleanQuery
defineHasParamClass "in_reply_to_status_id" ''StatusId 'readShow
defineHasParamClass "display_coordinates" ''Bool 'booleanQuery
defineHasParamClass "possibly_sensitive" ''Bool 'booleanQuery
defineHasParamClass "lang" ''Text 'utf8
defineHasParamClass "locale" ''Text 'utf8
defineHasParamClass "until" ''Day 'readShow
defineHasParamClass "skip_status" ''Bool 'booleanQuery
defineHasParamClass "follow" ''Bool 'booleanQuery

-- | converts 'UserParam' to 'HT.SimpleQuery'.
--
-- >>> mkUserParam $ UserIdParam 123456
-- [("user_id","123456")]
-- >>> mkUserParam $ ScreenNameParam "thimura"
-- [("screen_name","thimura")]
mkUserParam :: UserParam -> HT.SimpleQuery
mkUserParam (UserIdParam uid) =  [("user_id", readShow # uid)]
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
mkUserListParam (UserIdListParam uids) =  [("user_id", uids ^.. traversed . re readShow & S8.intercalate ",")]
mkUserListParam (ScreenNameListParam sns) = [("screen_name", S8.intercalate "," . map S8.pack $ sns)]

-- | converts 'ListParam' to 'HT.SimpleQuery'.
--
-- >>> mkListParam $ ListIdParam 123123
-- [("list_id","123123")]
-- >>> mkListParam $ ListNameParam "thimura/haskell"
-- [("slug","haskell"),("owner_screen_name","thimura")]
mkListParam :: ListParam -> HT.SimpleQuery
mkListParam (ListIdParam lid) =  [("list_id", readShow # lid)]
mkListParam (ListNameParam listname) =
    [("slug", S8.pack lstName),
     ("owner_screen_name", S8.pack screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln

