{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Twitter.Conduit.Parameters
       ( Parameters(..)
       , PV(..)
       , APIQuery
       , APIQueryItem
       , makeSimpleQuery


       , HasParam
       , count
       , sinceId
       , maxId
       , page
       , cursor
       , trimUser
       , excludeReplies
       , contributorDetails
       , includeEntities
       , includeUserEntities
       , includeRts
       , includeMyRetweet
       , inReplyToStatusId
       , displayCoordinates
       , possiblySensitive
       , lang
       , language
       , locale
       , filterLevel
       , stallWarnings
       , replies
       , until
       , skipStatus
       , follow
       , map
       , mediaIds

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

import Prelude hiding (map, until)
import qualified Prelude

data UserParam = UserIdParam UserId | ScreenNameParam String
               deriving (Show, Eq)
data UserListParam = UserIdListParam [UserId] | ScreenNameListParam [String]
                   deriving (Show, Eq)
data ListParam = ListIdParam Integer | ListNameParam String
               deriving (Show, Eq)
data MediaData = MediaFromFile FilePath
               | MediaRequestBody FilePath RequestBody

defineParamInteger "count"
defineParamInteger "since_id"
defineParamInteger "max_id"
defineParamInteger "page"
defineParamInteger "cursor"
defineParamBool "trim_user"
defineParamBool "exclude_replies"
defineParamBool "contributor_details"
defineParamBool "include_entities"
defineParamBool "include_user_entities"
defineParamBool "include_rts"
defineParamBool "include_my_retweet"
defineParamInteger "in_reply_to_status_id"
defineParamBool "display_coordinates"
defineParamBool "possibly_sensitive"
defineParamString "lang"
defineParamString "language"
defineParamString "locale"
defineParamString "filter_level"
defineParamBool "stall_warnings"
defineParamString "replies"
defineParamDay "until"
defineParamBool "skip_status"
defineParamBool "follow"
defineParamBool "map"
defineParamIntegerArray "media_ids"

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
