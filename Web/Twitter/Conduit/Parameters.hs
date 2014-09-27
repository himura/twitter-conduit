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
       , HasLanguageParam (..)
       , HasLocaleParam (..)
       , HasFilterLevelParam (..)
       , HasStallWarningsParam (..)
       , HasRepliesParam (..)
       , HasUntilParam (..)
       , HasSkipStatusParam (..)
       , HasFollowParam (..)
       , HasMapParam (..)
       , HasMediaIdsParam (..)

       , UserParam(..)
       , UserListParam(..)
       , ListParam(..)
       , MediaData(..)
       , mkUserParam
       , mkUserListParam
       , mkListParam
       ) where

import qualified Data.Text as T
import Network.HTTP.Client (RequestBody)
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Conduit.Request
import Web.Twitter.Types

data UserParam = UserIdParam UserId | ScreenNameParam String
               deriving (Show, Eq)
data UserListParam = UserIdListParam [UserId] | ScreenNameListParam [String]
                   deriving (Show, Eq)
data ListParam = ListIdParam Integer | ListNameParam String
               deriving (Show, Eq)
data MediaData = MediaFromFile FilePath
               | MediaRequestBody FilePath RequestBody

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
defineHasParamClassString "language"
defineHasParamClassString "locale"
defineHasParamClassString "filter_level"
defineHasParamClassBool "stall_warnings"
defineHasParamClassString "replies"
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
mkUserListParam (ScreenNameListParam sns) = [("screen_name", PVStringArray (Prelude.map T.pack sns))]

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
