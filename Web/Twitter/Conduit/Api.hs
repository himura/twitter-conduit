{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
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

data DirectMessages
directMessages :: APIRequest DirectMessages [DirectMessage]
directMessages = APIRequestGet (endpoint ++ "direct_messages.json") def
instance HasMaxIdParam (APIRequest DirectMessages [DirectMessage])

data FriendsIds
friendsIds :: UserParam -> APIRequest FriendsIds (WithCursor IdsCursorKey UserId)
friendsIds q = APIRequestGet (endpoint ++ "friends/ids.json") (mkUserParam q)

data FollowersIds
followersIds :: UserParam -> APIRequest FollowersIds (WithCursor IdsCursorKey UserId)
followersIds q = APIRequestGet (endpoint ++ "followers/ids.json") (mkUserParam q)

data FriendshipsCreate
friendshipsCreate :: UserParam -> APIRequest FriendshipsCreate User
friendshipsCreate user = APIRequestPost (endpoint ++ "friendships/create.json") (mkUserParam user)

data FriendsList
friendsList :: UserParam -> APIRequest FriendsList (WithCursor UsersCursorKey User)
friendsList q = APIRequestGet (endpoint ++ "friends/list.json") (mkUserParam q)

data UsersLookup
usersLookup :: UserListParam -> APIRequest UsersLookup [User]
usersLookup q = APIRequestGet (endpoint ++ "users/lookup.json") (mkUserListParam q)

data UsersShow
usersShow :: UserParam -> APIRequest UsersShow User
usersShow q = APIRequestGet (endpoint ++ "users/show.json") (mkUserParam q)

data FavoritesCreate
favoritesCreate :: StatusId -> APIRequest FavoritesCreate Status
favoritesCreate sid = APIRequestPost (endpoint ++ "favorites/create.json") [("id", showBS sid)]

data FavoritesDestroy
favoritesDestroy :: StatusId -> APIRequest FavoritesDestroy Status
favoritesDestroy sid = APIRequestPost (endpoint ++ "favorites/destroy.json") [("id", showBS sid)]

data ListMembers
listsMembers :: ListParam -> APIRequest ListMembers (WithCursor UsersCursorKey User)
listsMembers q = APIRequestGet (endpoint ++ "lists/members.json") (mkListParam q)
