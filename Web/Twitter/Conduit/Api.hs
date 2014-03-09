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
         Search
       , search

       -- * Direct Messages
       , DirectMessages
       , directMessages
       , DirectMessagesSent
       , directMessagesSent
       , DirectMessagesShow
       , directMessagesShow
       , DirectMessagesDestroy
       , directMessagesDestroy
       , DirectMessagesNew
       , directMessagesNew

       -- * Friends & Followers
       -- , friendshipsNoRetweetsIds
       , FriendsIds
       , friendsIds
       , FollowersIds
       , followersIds
       -- , friendshipsLookup
       -- , friendshipsIncoming
       -- , friendshipsOutgoing
       , FriendshipsCreate
       , friendshipsCreate
       -- , friendshipsDestroy
       -- , friendshipsUpdate
       -- , friendshipsShow
       , FriendsList
       , friendsList
       -- , followersList

       -- * Users
       -- , accountSettings
       , AccountVerifyCredentials
       , accountVerifyCredentials
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

       , UsersLookup
       , usersLookup
       , UsersShow
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
       , FavoritesDestroy
       , favoritesDestroy
       , FavoritesCreate
       , favoritesCreate

       -- * Lists
       -- , listsStatuses
       -- , listsMemberships
       -- , listsSubscribers
       -- , listsSubscribersShow
       -- , listsMembersShow
       , ListsMembers
       , listsMembers
       -- , lists
       -- , listsShow
       -- , listsSubscriptions
       ) where

import Web.Twitter.Types
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Cursor

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Default

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens

data Search
-- | Returns search query.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' ('search' \"search text\")
-- 'liftIO' . 'print' $ res ^. 'searchResultStatuses'
-- @
--
-- >>> search "search text"
-- APIRequestGet "https://api.twitter.com/1.1/search/tweets.json" [("q","search text")]
-- >>> search "search text" & lang ?~ "ja" & count ?~ 100
-- APIRequestGet "https://api.twitter.com/1.1/search/tweets.json" [("count","100"),("lang","ja"),("q","search text")]
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
-- | Returns query data which asks recent direct messages sent to the authenticating user.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' '$' 'directMessages' '&' 'count' '?~' 100
-- @
--
-- >>> directMessages
-- APIRequestGet "https://api.twitter.com/1.1/direct_messages.json" []
-- >>> directMessages & count ?~ 100
-- APIRequestGet "https://api.twitter.com/1.1/direct_messages.json" [("count","100")]
directMessages :: APIRequest DirectMessages [DirectMessage]
directMessages = APIRequestGet (endpoint ++ "direct_messages.json") def
deriveHasParamInstances ''DirectMessages
    [ "since_id"
    , "max_id"
    , "count"
    , "include_entities"
    , "skip_status"
    ]

data DirectMessagesSent
-- | Returns query data which asks recent direct messages sent by the authenticating user.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' '$' 'directMessagesSent' '&' 'count' '?~' 100
-- @
--
-- >>> directMessagesSent
-- APIRequestGet "https://api.twitter.com/1.1/direct_messages/sent.json" []
-- >>> directMessagesSent & count ?~ 100
-- APIRequestGet "https://api.twitter.com/1.1/direct_messages/sent.json" [("count","100")]
directMessagesSent :: APIRequest DirectMessagesSent [DirectMessage]
directMessagesSent = APIRequestGet (endpoint ++ "direct_messages/sent.json") def
deriveHasParamInstances ''DirectMessagesSent
    [ "since_id"
    , "max_id"
    , "count"
    , "include_entities"
    , "page"
    , "skip_status"
    ]

data DirectMessagesShow
-- | Returns query data which asks a single direct message, specified by an id parameter.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' '$' 'directMessagesShow' 1234567890
-- @
--
-- >>> directMessagesShow 1234567890
-- APIRequestGet "https://api.twitter.com/1.1/direct_messages/show.json" [("id","1234567890")]
directMessagesShow :: StatusId -> APIRequest DirectMessagesShow DirectMessage
directMessagesShow sId = APIRequestGet (endpoint ++ "direct_messages/show.json") [("id", showBS sId)]

data DirectMessagesDestroy
-- | Returns post data which destroys the direct message specified in the required ID parameter.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' '$' 'directMessagesDestroy' 1234567890
-- @
--
-- >>> directMessagesDestroy 1234567890
-- APIRequestPost "https://api.twitter.com/1.1/direct_messages/destroy.json" [("id","1234567890")]
directMessagesDestroy :: StatusId -> APIRequest DirectMessagesDestroy DirectMessage
directMessagesDestroy sId = APIRequestPost (endpoint ++ "direct_messages/destroy.json") [("id", showBS sId)]
deriveHasParamInstances ''DirectMessagesDestroy
    [ "include_entities"
    ]

data DirectMessagesNew
-- | Returns post data which sends a new direct message to the specified user from the authenticating user.
--
-- You can perform a post using 'call':
--
-- @
-- res <- 'call' '$' 'directMessagesNew' (ScreenNameParam \"thimura\") \"Hello DM\"
-- @
--
-- >>> directMessagesNew (ScreenNameParam "thimura") "Hello DM"
-- APIRequestPost "https://api.twitter.com/1.1/direct_messages/new.json" [("text","Hello DM"),("screen_name","thimura")]
-- >>> directMessagesNew (UserIdParam 69179963) "Hello thimura! by UserId"
-- APIRequestPost "https://api.twitter.com/1.1/direct_messages/new.json" [("text","Hello thimura! by UserId"),("user_id","69179963")]
directMessagesNew :: UserParam -> T.Text -> APIRequest DirectMessagesNew DirectMessage
directMessagesNew q msg = APIRequestPost (endpoint ++ "direct_messages/new.json") (("text", T.encodeUtf8 msg):mkUserParam q)

data FriendsIds
-- | Returns query data which asks a collection of user IDs for every user the specified user is following.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' '$' 'friendsIds' ('ScreenNameParam' \"thimura\")
-- @
--
-- Or, you can iterate with 'sourceWithCursor':
--
-- @
-- 'sourceWithCursor' ('friendsIds' ('ScreenNameParam' \"thimura\")) $$ CL.consume
-- @
--
-- >>> friendsIds (ScreenNameParam "thimura")
-- APIRequestGet "https://api.twitter.com/1.1/friends/ids.json" [("screen_name","thimura")]
-- >>> friendsIds (ScreenNameParam "thimura") & count ?~ 5000
-- APIRequestGet "https://api.twitter.com/1.1/friends/ids.json" [("count","5000"),("screen_name","thimura")]
friendsIds :: UserParam -> APIRequest FriendsIds (WithCursor IdsCursorKey UserId)
friendsIds q = APIRequestGet (endpoint ++ "friends/ids.json") (mkUserParam q)
deriveHasParamInstances ''FriendsIds
    [ "cursor"
    -- , "stringify_ids" -- (needless)
    , "count"
    ]

data FollowersIds
-- | Returns query data which asks a collection of user IDs for every user following the specified user.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' '$' 'followersIds' ('ScreenNameParam' \"thimura\")
-- @
--
-- Or, you can iterate with 'sourceWithCursor':
--
-- @
-- 'sourceWithCursor' ('followersIds' ('ScreenNameParam' \"thimura\")) $$ CL.consume
-- @
--
-- >>> followersIds (ScreenNameParam "thimura")
-- APIRequestGet "https://api.twitter.com/1.1/followers/ids.json" [("screen_name","thimura")]
-- >>> followersIds (ScreenNameParam "thimura") & count ?~ 5000
-- APIRequestGet "https://api.twitter.com/1.1/followers/ids.json" [("count","5000"),("screen_name","thimura")]
followersIds :: UserParam -> APIRequest FollowersIds (WithCursor IdsCursorKey UserId)
followersIds q = APIRequestGet (endpoint ++ "followers/ids.json") (mkUserParam q)
deriveHasParamInstances ''FollowersIds
    [ "cursor"
    -- , "stringify_ids" -- (needless)
    , "count"
    ]

data FriendshipsCreate
-- | Returns post data which follows the user specified in the ID parameter.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' '$' 'friendshipsCreate' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> friendshipsCreate (ScreenNameParam "thimura")
-- APIRequestPost "https://api.twitter.com/1.1/friendships/create.json" [("screen_name","thimura")]
-- >>> friendshipsCreate (UserIdParam 69179963)
-- APIRequestPost "https://api.twitter.com/1.1/friendships/create.json" [("user_id","69179963")]
friendshipsCreate :: UserParam -> APIRequest FriendshipsCreate User
friendshipsCreate user = APIRequestPost (endpoint ++ "friendships/create.json") (mkUserParam user)
deriveHasParamInstances ''FriendshipsCreate
    [ "follow"
    ]

data FriendsList
-- | Returns query data which asks a cursored collection of user objects for every user the specified users is following.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' '$' 'friendsList' ('ScreenNameParam' \"thimura\")
-- @
--
-- Or, you can iterate with 'sourceWithCursor':
--
-- @
-- 'sourceWithCursor' ('friendsList' ('ScreenNameParam' \"thimura\")) $$ CL.consume
-- @
--
-- >>> friendsList (ScreenNameParam "thimura")
-- APIRequestGet "https://api.twitter.com/1.1/friends/list.json" [("screen_name","thimura")]
-- >>> friendsList (UserIdParam 69179963)
-- APIRequestGet "https://api.twitter.com/1.1/friends/list.json" [("user_id","69179963")]
friendsList :: UserParam -> APIRequest FriendsList (WithCursor UsersCursorKey User)
friendsList q = APIRequestGet (endpoint ++ "friends/list.json") (mkUserParam q)
deriveHasParamInstances ''FriendsList
    [ "cursor"
    , "count"
    , "skip_status"
    , "include_user_entities"
    ]

data AccountVerifyCredentials
-- | Returns query data asks that the credential is valid.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' '$' 'accountVerifyCredentials'
-- @
--
-- >>> accountVerifyCredentials
-- APIRequestGet "https://api.twitter.com/1.1/account/verify_credentials.json" []
accountVerifyCredentials :: APIRequest AccountVerifyCredentials User
accountVerifyCredentials = APIRequestGet (endpoint ++ "account/verify_credentials.json") []
deriveHasParamInstances ''AccountVerifyCredentials
    [ "include_entities"
    , "skip_status"
    ]

data UsersLookup
-- | Returns query data asks user objects.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' '$' 'usersLookup' ('ScreenNameListParam' [\"thimura\", \"twitterapi\"])
-- @
--
-- >>> usersLookup (ScreenNameListParam ["thimura", "twitterapi"])
-- APIRequestGet "https://api.twitter.com/1.1/users/lookup.json" [("screen_name","thimura,twitterapi")]
usersLookup :: UserListParam -> APIRequest UsersLookup [User]
usersLookup q = APIRequestGet (endpoint ++ "users/lookup.json") (mkUserListParam q)
deriveHasParamInstances ''UsersLookup
    [ "include_entities"
    ]

data UsersShow
-- | Returns query data asks the user specified by user id or screen name parameter.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' '$' 'usersShow' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> usersShow (ScreenNameParam "thimura")
-- APIRequestGet "https://api.twitter.com/1.1/users/show.json" [("screen_name","thimura")]
usersShow :: UserParam -> APIRequest UsersShow User
usersShow q = APIRequestGet (endpoint ++ "users/show.json") (mkUserParam q)
deriveHasParamInstances ''UsersShow
    [ "include_entities"
    ]

data FavoritesCreate
-- | Returns post data which favorites the status specified in the ID parameter as the authenticating user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' '$' 'favoritesCreate' 1234567890
-- @
--
-- >>> favoritesCreate 1234567890
-- APIRequestPost "https://api.twitter.com/1.1/favorites/create.json" [("id","1234567890")]
favoritesCreate :: StatusId -> APIRequest FavoritesCreate Status
favoritesCreate sid = APIRequestPost (endpoint ++ "favorites/create.json") [("id", showBS sid)]
deriveHasParamInstances ''FavoritesCreate
    [ "include_entities"
    ]

data FavoritesDestroy
-- | Returns post data unfavorites the status specified in the ID paramter as the authenticating user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' '$' 'favoritesDestroy' 1234567890
-- @
--
-- >>> favoritesDestroy 1234567890
-- APIRequestPost "https://api.twitter.com/1.1/favorites/destroy.json" [("id","1234567890")]
favoritesDestroy :: StatusId -> APIRequest FavoritesDestroy Status
favoritesDestroy sid = APIRequestPost (endpoint ++ "favorites/destroy.json") [("id", showBS sid)]
deriveHasParamInstances ''FavoritesDestroy
    [ "include_entities"
    ]

data ListsMembers
-- | Returns query data asks the members of the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' '$' 'listsMembers' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsMembers (ListNameParam "thimura/haskell")
-- APIRequestGet "https://api.twitter.com/1.1/lists/members.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsMembers (ListIdParam 20849097)
-- APIRequestGet "https://api.twitter.com/1.1/lists/members.json" [("list_id","20849097")]
listsMembers :: ListParam -> APIRequest ListsMembers (WithCursor UsersCursorKey User)
listsMembers q = APIRequestGet (endpoint ++ "lists/members.json") (mkListParam q)
deriveHasParamInstances ''ListsMembers
    [ "cursor"
    , "skip_status"
    ]

