{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Twitter.Conduit.Parameters
       ( Parameters(..)
       , PV(..)
       , APIQuery
       , APIQueryItem
       , makeSimpleQuery

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
       , HasMapParam (..)
       , HasMediaIdsParam (..)

       , UserParam(..)
       , UserListParam(..)
       , ListParam(..)
       , mkUserParam
       , mkUserListParam
       , mkListParam
       ) where

import Prelude as P
import Web.Twitter.Conduit.Parameters.Internal
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Types
import qualified Data.Text as T

data UserParam = UserIdParam UserId | ScreenNameParam String
               deriving (Show, Eq)
data UserListParam = UserIdListParam [UserId] | ScreenNameListParam [String]
                   deriving (Show, Eq)
data ListParam = ListIdParam Integer | ListNameParam String
               deriving (Show, Eq)

defineHasParamClassInteger "count"
defineHasParamClassInteger "since_id"
defineHasParamClassInteger "max_id"
defineHasParamClassInteger "page"
defineHasParamClassInteger "cursor"
defineHasParamClassBool "trim_user"
defineHasParamClassBool "exclude_replies"
defineHasParamClassBool "contributor_details"
defineHasParamClassBool "include_entities"
defineHasParamClassBool "include_user_entities"
defineHasParamClassBool "include_rts"
defineHasParamClassBool "include_my_retweet"
defineHasParamClassInteger "in_reply_to_status_id"
defineHasParamClassBool "display_coordinates"
defineHasParamClassBool "possibly_sensitive"
defineHasParamClassString "lang"
defineHasParamClassString "locale"
defineHasParamClassDay "until"
defineHasParamClassBool "skip_status"
defineHasParamClassBool "follow"
defineHasParamClassBool "map"
defineHasParamClassIntegerArray "media_ids"

-- | converts 'UserParam' to 'HT.SimpleQuery'.
--
-- >>> makeSimpleQuery . mkUserParam $ UserIdParam 123456
-- [("user_id","123456")]
-- >>> makeSimpleQuery . mkUserParam $ ScreenNameParam "thimura"
-- [("screen_name","thimura")]
mkUserParam :: UserParam -> APIQuery
mkUserParam (UserIdParam uid) =  [("user_id", PVInteger uid)]
mkUserParam (ScreenNameParam sn) = [("screen_name", PVString . T.pack $ sn)]

-- | converts 'UserListParam' to 'HT.SimpleQuery'.
--
-- >>> makeSimpleQuery . mkUserListParam $ UserIdListParam [123456]
-- [("user_id","123456")]
-- >>> makeSimpleQuery . mkUserListParam $ UserIdListParam [123456, 654321]
-- [("user_id","123456,654321")]
-- >>> makeSimpleQuery . mkUserListParam $ ScreenNameListParam ["thimura", "NikaidouShinku"]
-- [("screen_name","thimura,NikaidouShinku")]
mkUserListParam :: UserListParam -> APIQuery
mkUserListParam (UserIdListParam uids) =  [("user_id", PVIntegerArray uids)]
mkUserListParam (ScreenNameListParam sns) = [("screen_name", PVStringArray (P.map T.pack sns))]

-- | converts 'ListParam' to 'HT.SimpleQuery'.
--
-- >>> makeSimpleQuery . mkListParam $ ListIdParam 123123
-- [("list_id","123123")]
-- >>> makeSimpleQuery . mkListParam $ ListNameParam "thimura/haskell"
-- [("slug","haskell"),("owner_screen_name","thimura")]
mkListParam :: ListParam -> APIQuery
mkListParam (ListIdParam lid) =  [("list_id", PVInteger lid)]
mkListParam (ListNameParam listname) =
    [("slug", PVString (T.pack lstName)),
     ("owner_screen_name", PVString (T.pack screenName))]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln
