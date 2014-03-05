{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Conduit.Api
       (
       -- * Search
         search

       -- * Direct Messages
       , directMessages
       -- , directMessagesSent
       -- , directMessagesShow
       -- , directMessagesDestroy
       -- , directMessagesNew

       -- * Friends & Followers
       -- , friendshipsNoRetweetsIds
       , friendsIds
       , followersIds
       -- , friendshipsLookup
       -- , friendshipsIncoming
       -- , friendshipsOutgoing
       , friendshipsCreate
       -- , friendshipsDestroy
       -- , friendshipsUpdate
       -- , friendshipsShow
       , friendsList
       -- , followersList

       -- * Users
       -- , accountSettings
       -- , accountVerifyCredentials
       -- , accountSettingsUpdate
       -- , accountUpdateDeliveryDevice
       -- , accountUpdateProfile
       -- , accountUpdateProfileBackgroundImage
       -- , accountUpdateProfileColors
       -- , accoutUpdateProfileImage
       -- , blocksList
       -- , blocksIds
       -- , blocksCreate
       -- , blocksDestroy

       , usersLookup
       , usersShow
       -- , usersSearch
       -- , usersContributees
       -- , usersContributors
       -- , accuntRemoveProfileBanner
       -- , accuntUpdateProfileBanner
       -- , accuntProfileBanner

       -- * Suggested Users
       -- , usersSuggestionsSlug
       -- , usersSuggestions
       -- , usersSuggestionsSlugMembers

       -- * Favorites
       -- , favoritesList
       , favoritesDestroy
       , favoritesCreate

       -- * Lists
       -- , listsStatuses
       -- , listsMemberships
       -- , listsSubscribers
       -- , listsSubscribersShow
       -- , listsMembersShow
       , listsMembers
       -- , lists
       -- , listsShow
       -- , listsSubscriptions
       ) where

import Web.Twitter.Types
import Web.Twitter.Conduit.Param
import Web.Twitter.Conduit.Utils
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Cursor

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Default

data Search
search :: T.Text -- ^ search string
       -> APIRequest Search (SearchResult [SearchStatus])
search q = APIRequestGet (endpoint ++ "search/tweets.json") [("q", T.encodeUtf8 q)]
deriveHasParamInstances ''Search
    [ "lang"
    , "locale"
    -- , "result_type"
    , "count"
    , "until"
    , "since_id"
    , "max_id"
    , "include_entities"
    -- , "callback"  (needless)
    ]

data DirectMessages
directMessages :: APIRequest DirectMessages [DirectMessage]
directMessages = APIRequestGet (endpoint ++ "direct_messages.json") def
deriveHasParamInstances ''DirectMessages
    [ "since_id"
    , "max_id"
    , "count"
    , "include_entities"
    , "skip_status"
    ]

data FriendsIds
friendsIds :: UserParam -> APIRequest FriendsIds (WithCursor IdsCursorKey UserId)
friendsIds q = APIRequestGet (endpoint ++ "friends/ids.json") (mkUserParam q)
deriveHasParamInstances ''FriendsIds
    [ "cursor"
    -- , "stringify_ids" -- (needless)
    , "count"
    ]

data FollowersIds
followersIds :: UserParam -> APIRequest FollowersIds (WithCursor IdsCursorKey UserId)
followersIds q = APIRequestGet (endpoint ++ "followers/ids.json") (mkUserParam q)
deriveHasParamInstances ''FollowersIds
    [ "cursor"
    -- , "stringify_ids" -- (needless)
    , "count"
    ]

data FriendshipsCreate
friendshipsCreate :: UserParam -> APIRequest FriendshipsCreate User
friendshipsCreate user = APIRequestPost (endpoint ++ "friendships/create.json") (mkUserParam user)
deriveHasParamInstances ''FriendshipsCreate
    [ "follow"
    ]

data FriendsList
friendsList :: UserParam -> APIRequest FriendsList (WithCursor UsersCursorKey User)
friendsList q = APIRequestGet (endpoint ++ "friends/list.json") (mkUserParam q)
deriveHasParamInstances ''FriendsList
    [ "cursor"
    , "count"
    , "skip_status"
    , "include_user_entities"
    ]

data UsersLookup
usersLookup :: UserListParam -> APIRequest UsersLookup [User]
usersLookup q = APIRequestGet (endpoint ++ "users/lookup.json") (mkUserListParam q)
deriveHasParamInstances ''UsersLookup
    [ "include_entities"
    ]

data UsersShow
usersShow :: UserParam -> APIRequest UsersShow User
usersShow q = APIRequestGet (endpoint ++ "users/show.json") (mkUserParam q)
deriveHasParamInstances ''UsersShow
    [ "include_entities"
    ]

data FavoritesCreate
favoritesCreate :: StatusId -> APIRequest FavoritesCreate Status
favoritesCreate sid = APIRequestPost (endpoint ++ "favorites/create.json") [("id", showBS sid)]
deriveHasParamInstances ''FavoritesCreate
    [ "include_entities"
    ]

data FavoritesDestroy
favoritesDestroy :: StatusId -> APIRequest FavoritesDestroy Status
favoritesDestroy sid = APIRequestPost (endpoint ++ "favorites/destroy.json") [("id", showBS sid)]
deriveHasParamInstances ''FavoritesDestroy
    [ "include_entities"
    ]

data ListsMembers
listsMembers :: ListParam -> APIRequest ListsMembers (WithCursor UsersCursorKey User)
listsMembers q = APIRequestGet (endpoint ++ "lists/members.json") (mkListParam q)
deriveHasParamInstances ''ListsMembers
    [ "cursor"
    , "skip_status"
    ]

