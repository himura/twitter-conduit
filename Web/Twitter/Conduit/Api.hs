{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Web.Twitter.Conduit.Api
       (
       -- * Search
         SearchTweets
       , searchTweets
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
       , FriendshipsNoRetweetsIds
       , friendshipsNoRetweetsIds
       , FriendsIds
       , friendsIds
       , FollowersIds
       , followersIds
       , FriendshipsIncoming
       , friendshipsIncoming
       , FriendshipsOutgoing
       , friendshipsOutgoing
       , FriendshipsCreate
       , friendshipsCreate
       , FriendshipsDestroy
       , friendshipsDestroy
       -- , friendshipsUpdate
       -- , friendshipsShow
       , FriendsList
       , friendsList
       , FollowersList
       , followersList
       -- , friendshipsLookup

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
       -- , usersProfileBanner
       -- , mutesUsersCreate
       -- , mutesUsersDestroy
       -- , mutesUsersIds
       -- , mutesUsersList

       -- * Suggested Users
       -- , usersSuggestionsSlug
       -- , usersSuggestions
       -- , usersSuggestionsSlugMembers

       -- * Favorites
       , FavoritesList
       , favoritesList
       , FavoritesDestroy
       , favoritesDestroy
       , FavoritesCreate
       , favoritesCreate

       -- * Lists
       -- , listsList
       , ListsStatuses
       , listsStatuses
       , ListsMembersDestroy
       , listsMembersDestroy
       , ListsMemberships
       , listsMemberships
       , ListsSubscribers
       , listsSubscribers
       -- , listsSubscribersCreate
       -- , listsSubscribersShow
       -- , listsSubscribersDestroy
       -- , listsMembersCreateAll
       -- , listsMembersShow
       , ListsMembers
       , listsMembers
       , ListsMembersCreate
       , listsMembersCreate
       , ListsDestroy
       , listsDestroy
       , ListsUpdate
       , listsUpdate
       , ListsCreate
       , listsCreate
       , ListsShow
       , listsShow
       , ListsSubscriptions
       , listsSubscriptions
       -- , listsMembersDestroyAll
       , ListsOwnerships
       , listsOwnerships

       -- * Saved Searches
       -- savedSearchesList
       -- savedSearchesShowId
       -- savedSearchesCreate
       -- savedSearchesDestroyId

       -- * Places & Geo
       -- geoIdPlaceId
       -- geoReverseGeocode
       -- geoSearch
       -- geoSimilarPlaces
       -- geoPlace

       -- * media
       , MediaUpload
       , mediaUpload
       ) where

import Web.Twitter.Types
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Cursor

import Network.HTTP.Client.MultipartFormData
import qualified Data.Text as T
import Data.Default

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens

data SearchTweets
-- | Returns search query.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' ('searchTweets' \"search text\")
-- 'liftIO' . 'print' $ res ^. 'searchResultStatuses'
-- @
--
-- >>> searchTweets "search text"
-- APIRequestGet "https://api.twitter.com/1.1/search/tweets.json" [("q","search text")]
-- >>> searchTweets "search text" & lang ?~ "ja" & count ?~ 100
-- APIRequestGet "https://api.twitter.com/1.1/search/tweets.json" [("count","100"),("lang","ja"),("q","search text")]
searchTweets :: T.Text -- ^ search string
             -> APIRequest SearchTweets (SearchResult [SearchStatus])
searchTweets q = APIRequestGet (endpoint ++ "search/tweets.json") [("q", PVString q)]
deriveHasParamInstances ''SearchTweets
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

-- | Alias of 'searchTweets', for backward compatibility
search :: T.Text -- ^ search string
       -> APIRequest SearchTweets (SearchResult [SearchStatus])
search = searchTweets

data DirectMessages
-- | Returns query data which asks recent direct messages sent to the authenticating user.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'directMessages' '&' 'count' '?~' 100
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
-- res <- 'call' twInfo mgr '$' 'directMessagesSent' '&' 'count' '?~' 100
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
-- res <- 'call' twInfo mgr '$' 'directMessagesShow' 1234567890
-- @
--
-- >>> directMessagesShow 1234567890
-- APIRequestGet "https://api.twitter.com/1.1/direct_messages/show.json" [("id","1234567890")]
directMessagesShow :: StatusId -> APIRequest DirectMessagesShow DirectMessage
directMessagesShow sId = APIRequestGet (endpoint ++ "direct_messages/show.json") [("id", PVInteger sId)]

data DirectMessagesDestroy
-- | Returns post data which destroys the direct message specified in the required ID parameter.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'directMessagesDestroy' 1234567890
-- @
--
-- >>> directMessagesDestroy 1234567890
-- APIRequestPost "https://api.twitter.com/1.1/direct_messages/destroy.json" [("id","1234567890")]
directMessagesDestroy :: StatusId -> APIRequest DirectMessagesDestroy DirectMessage
directMessagesDestroy sId = APIRequestPost (endpoint ++ "direct_messages/destroy.json") [("id", PVInteger sId)]
deriveHasParamInstances ''DirectMessagesDestroy
    [ "include_entities"
    ]

data DirectMessagesNew
-- | Returns post data which sends a new direct message to the specified user from the authenticating user.
--
-- You can perform a post using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'directMessagesNew' (ScreenNameParam \"thimura\") \"Hello DM\"
-- @
--
-- >>> directMessagesNew (ScreenNameParam "thimura") "Hello DM"
-- APIRequestPost "https://api.twitter.com/1.1/direct_messages/new.json" [("text","Hello DM"),("screen_name","thimura")]
-- >>> directMessagesNew (UserIdParam 69179963) "Hello thimura! by UserId"
-- APIRequestPost "https://api.twitter.com/1.1/direct_messages/new.json" [("text","Hello thimura! by UserId"),("user_id","69179963")]
directMessagesNew :: UserParam -> T.Text -> APIRequest DirectMessagesNew DirectMessage
directMessagesNew q msg = APIRequestPost (endpoint ++ "direct_messages/new.json") (("text", PVString msg):mkUserParam q)

data FriendshipsNoRetweetsIds
-- | Returns a collection of user_ids that the currently authenticated user does not want to receive retweets from.
--
-- You can perform a request using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendshipsNoRetweetsIds'
-- @
--
-- >>> friendshipsNoRetweetsIds
-- APIRequestGet "https://api.twitter.com/1.1/friendships/no_retweets/ids.json" []
friendshipsNoRetweetsIds :: APIRequest FriendshipsNoRetweetsIds [UserId]
friendshipsNoRetweetsIds = APIRequestGet (endpoint ++ "friendships/no_retweets/ids.json") []

data FriendsIds
-- | Returns query data which asks a collection of user IDs for every user the specified user is following.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendsIds' ('ScreenNameParam' \"thimura\")
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
-- res <- 'call' twInfo mgr '$' 'followersIds' ('ScreenNameParam' \"thimura\")
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

data FriendshipsIncoming
-- | Returns a collection of numeric IDs for every user who has a pending request to follow the authenticating user.
--
-- You can perform a request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendshipsIncoming'
-- @
--
-- Or, you can iterate with 'sourceWithCursor':
--
-- @
-- 'sourceWithCursor' 'friendshipsIncoming' $$ CL.consume
-- @
--
-- >>> friendshipsIncoming
-- APIRequestGet "https://api.twitter.com/1.1/friendships/incoming.json" []
friendshipsIncoming :: APIRequest FriendshipsIncoming (WithCursor IdsCursorKey UserId)
friendshipsIncoming = APIRequestGet (endpoint ++ "friendships/incoming.json") def
deriveHasParamInstances ''FriendshipsIncoming
    [ "cursor"
    -- , "stringify_ids" -- (needless)
    ]

data FriendshipsOutgoing
-- | Returns a collection of numeric IDs for every protected user for whom the authenticating user has a pending follow request.
--
-- You can perform a request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendshipsOutgoing'
-- @
--
-- Or, you can iterate with 'sourceWithCursor':
--
-- @
-- 'sourceWithCursor' 'friendshipsOutgoing' $$ CL.consume
-- @
--
-- >>> friendshipsOutgoing
-- APIRequestGet "https://api.twitter.com/1.1/friendships/outgoing.json" []
friendshipsOutgoing :: APIRequest FriendshipsOutgoing (WithCursor IdsCursorKey UserId)
friendshipsOutgoing = APIRequestGet (endpoint ++ "friendships/outgoing.json") def
deriveHasParamInstances ''FriendshipsOutgoing
    [ "cursor"
    -- , "stringify_ids" -- (needless)
    ]

data FriendshipsCreate
-- | Returns post data which follows the user specified in the ID parameter.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendshipsCreate' ('ScreenNameParam' \"thimura\")
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

data FriendshipsDestroy
-- | Returns post data which unfollows the user specified in the ID parameter.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendshipsDestroy' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> friendshipsDestroy (ScreenNameParam "thimura")
-- APIRequestPost "https://api.twitter.com/1.1/friendships/destroy.json" [("screen_name","thimura")]
-- >>> friendshipsDestroy (UserIdParam 69179963)
-- APIRequestPost "https://api.twitter.com/1.1/friendships/destroy.json" [("user_id","69179963")]
friendshipsDestroy :: UserParam -> APIRequest FriendshipsDestroy User
friendshipsDestroy user = APIRequestPost (endpoint ++ "friendships/destroy.json") (mkUserParam user)

data FriendsList
-- | Returns query data which asks a cursored collection of user objects for every user the specified users is following.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendsList' ('ScreenNameParam' \"thimura\")
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

data FollowersList
-- | Returns query data which asks a cursored collection of user objects for users following the specified user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'followersList' ('ScreenNameParam' \"thimura\")
-- @
--
-- Or, you can iterate with 'sourceWithCursor':
--
-- @
-- 'sourceWithCursor' ('followersList' ('ScreenNameParam' \"thimura\")) $$ CL.consume
-- @
--
-- >>> followersList (ScreenNameParam "thimura")
-- APIRequestGet "https://api.twitter.com/1.1/followers/list.json" [("screen_name","thimura")]
-- >>> followersList (UserIdParam 69179963)
-- APIRequestGet "https://api.twitter.com/1.1/followers/list.json" [("user_id","69179963")]
followersList :: UserParam -> APIRequest FollowersList (WithCursor UsersCursorKey User)
followersList q = APIRequestGet (endpoint ++ "followers/list.json") (mkUserParam q)
deriveHasParamInstances ''FollowersList
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
-- res <- 'call' twInfo mgr '$' 'accountVerifyCredentials'
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
-- res <- 'call' twInfo mgr '$' 'usersLookup' ('ScreenNameListParam' [\"thimura\", \"twitterapi\"])
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
-- res <- 'call' twInfo mgr '$' 'usersShow' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> usersShow (ScreenNameParam "thimura")
-- APIRequestGet "https://api.twitter.com/1.1/users/show.json" [("screen_name","thimura")]
usersShow :: UserParam -> APIRequest UsersShow User
usersShow q = APIRequestGet (endpoint ++ "users/show.json") (mkUserParam q)
deriveHasParamInstances ''UsersShow
    [ "include_entities"
    ]

data FavoritesList
-- | Returns the 20 most recent Tweets favorited by the specified user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'favoritesList' (ScreenNameParam \"thimura\")
-- @
--
-- >>> favoritesList Nothing
-- APIRequestGet "https://api.twitter.com/1.1/favorites/list.json" []
-- >>> favoritesList (Just (ScreenNameParam "thimura"))
-- APIRequestGet "https://api.twitter.com/1.1/favorites/list.json" [("screen_name","thimura")]
-- >>> favoritesList (Just (UserIdParam 69179963))
-- APIRequestGet "https://api.twitter.com/1.1/favorites/list.json" [("user_id","69179963")]
favoritesList :: Maybe UserParam -> APIRequest FavoritesList [Status]
favoritesList mbuser = APIRequestGet (endpoint ++ "favorites/list.json") (mkParam mbuser)
  where
    mkParam Nothing = []
    mkParam (Just usr) = mkUserParam usr
deriveHasParamInstances ''FavoritesList
    [ "count"
    , "since_id"
    , "max_id"
    , "include_entities"
    ]

data FavoritesCreate
-- | Returns post data which favorites the status specified in the ID parameter as the authenticating user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'favoritesCreate' 1234567890
-- @
--
-- >>> favoritesCreate 1234567890
-- APIRequestPost "https://api.twitter.com/1.1/favorites/create.json" [("id","1234567890")]
favoritesCreate :: StatusId -> APIRequest FavoritesCreate Status
favoritesCreate sid = APIRequestPost (endpoint ++ "favorites/create.json") [("id", PVInteger sid)]
deriveHasParamInstances ''FavoritesCreate
    [ "include_entities"
    ]

data FavoritesDestroy
-- | Returns post data unfavorites the status specified in the ID paramter as the authenticating user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'favoritesDestroy' 1234567890
-- @
--
-- >>> favoritesDestroy 1234567890
-- APIRequestPost "https://api.twitter.com/1.1/favorites/destroy.json" [("id","1234567890")]
favoritesDestroy :: StatusId -> APIRequest FavoritesDestroy Status
favoritesDestroy sid = APIRequestPost (endpoint ++ "favorites/destroy.json") [("id", PVInteger sid)]
deriveHasParamInstances ''FavoritesDestroy
    [ "include_entities"
    ]

data ListsStatuses
-- | Returns the query parameter which fetches a timeline of tweets authored by members of the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsStatuses' ('ListNameParam' "thimura/haskell")
-- @
--
-- If you need more statuses, you can obtain those by using 'sourceWithMaxId':
-- @
-- res <- sourceWithMaxId ('listsStatuses' ('ListNameParam' "thimura/haskell") & count ?~ 200) $$ CL.take 1000
-- @
--
-- >>> listsStatuses (ListNameParam "thimura/haskell")
-- APIRequestGet "https://api.twitter.com/1.1/lists/statuses.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsStatuses (ListIdParam 20849097)
-- APIRequestGet "https://api.twitter.com/1.1/lists/statuses.json" [("list_id","20849097")]
listsStatuses :: ListParam -> APIRequest ListsStatuses [Status]
listsStatuses q = APIRequestGet (endpoint ++ "lists/statuses.json") (mkListParam q)
deriveHasParamInstances ''ListsStatuses
    [ "since_id"
    , "max_id"
    , "count"
    , "include_entities"
    , "include_rts"
    ]

data ListsMembersDestroy
-- | Returns the post parameter which removes the specified member from the list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsMembersDestroy' ('ListNameParam' "thimura/haskell") ('ScreenNameParam' "thimura")
-- @
--
-- >>> listsMembersDestroy (ListNameParam "thimura/haskell") (ScreenNameParam "thimura")
-- APIRequestPost "https://api.twitter.com/1.1/lists/members/destroy.json" [("slug","haskell"),("owner_screen_name","thimura"),("screen_name","thimura")]
-- >>> listsMembersDestroy (ListIdParam 20849097) (UserIdParam 69179963)
-- APIRequestPost "https://api.twitter.com/1.1/lists/members/destroy.json" [("list_id","20849097"),("user_id","69179963")]
listsMembersDestroy :: ListParam -> UserParam -> APIRequest ListsMembersDestroy List
listsMembersDestroy list user = APIRequestPost (endpoint ++ "lists/members/destroy.json") (mkListParam list ++ mkUserParam user)

data ListsMemberships
-- | Returns the request parameters which asks the lists the specified user has been added to.
-- If 'UserParam' are not provided, the memberships for the authenticating user are returned.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsMemberships' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsMemberships Nothing
-- APIRequestGet "https://api.twitter.com/1.1/lists/memberships.json" []
-- >>> listsMemberships (Just (ScreenNameParam "thimura"))
-- APIRequestGet "https://api.twitter.com/1.1/lists/memberships.json" [("screen_name","thimura")]
-- >>> listsMemberships (Just (UserIdParam 69179963))
-- APIRequestGet "https://api.twitter.com/1.1/lists/memberships.json" [("user_id","69179963")]
listsMemberships :: Maybe UserParam -> APIRequest ListsMemberships (WithCursor ListsCursorKey List)
listsMemberships q = APIRequestGet (endpoint ++ "lists/memberships.json") $ maybe [] mkUserParam q
deriveHasParamInstances ''ListsMemberships
    [ "cursor"
    , "count"
    ]

data ListsSubscribers
-- | Returns the request parameter which asks the subscribers of the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsSubscribers' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsSubscribers (ListNameParam "thimura/haskell")
-- APIRequestGet "https://api.twitter.com/1.1/lists/subscribers.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsSubscribers (ListIdParam 20849097)
-- APIRequestGet "https://api.twitter.com/1.1/lists/subscribers.json" [("list_id","20849097")]
listsSubscribers :: ListParam -> APIRequest ListsSubscribers (WithCursor UsersCursorKey User)
listsSubscribers q = APIRequestGet (endpoint ++ "lists/subscribers.json") (mkListParam q)
deriveHasParamInstances ''ListsSubscribers
    [ "cursor"
    , "count"
    , "skip_status"
    ]

data ListsSubscriptions
-- | Returns the request parameter which obtains a collection of the lists the specified user is subscribed to.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsSubscriptions' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsSubscriptions Nothing
-- APIRequestGet "https://api.twitter.com/1.1/lists/subscriptions.json" []
-- >>> listsSubscriptions (Just (ScreenNameParam "thimura"))
-- APIRequestGet "https://api.twitter.com/1.1/lists/subscriptions.json" [("screen_name","thimura")]
-- >>> listsSubscriptions (Just (UserIdParam 69179963))
-- APIRequestGet "https://api.twitter.com/1.1/lists/subscriptions.json" [("user_id","69179963")]
listsSubscriptions :: Maybe UserParam -> APIRequest ListsSubscriptions (WithCursor ListsCursorKey List)
listsSubscriptions q = APIRequestGet (endpoint ++ "lists/subscriptions.json") $ maybe [] mkUserParam q
deriveHasParamInstances ''ListsSubscriptions
    [ "cursor"
    , "count"
    ]

data ListsOwnerships
-- | Returns the request parameter which asks the lists owned by the specified Twitter user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsOwnerships' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsOwnerships Nothing
-- APIRequestGet "https://api.twitter.com/1.1/lists/ownerships.json" []
-- >>> listsOwnerships (Just (ScreenNameParam "thimura"))
-- APIRequestGet "https://api.twitter.com/1.1/lists/ownerships.json" [("screen_name","thimura")]
-- >>> listsOwnerships (Just (UserIdParam 69179963))
-- APIRequestGet "https://api.twitter.com/1.1/lists/ownerships.json" [("user_id","69179963")]
listsOwnerships :: Maybe UserParam -> APIRequest ListsOwnerships (WithCursor ListsCursorKey List)
listsOwnerships q = APIRequestGet (endpoint ++ "lists/ownerships.json") $ maybe [] mkUserParam q
deriveHasParamInstances ''ListsOwnerships
    [ "cursor"
    , "count"
    ]

data ListsMembers
-- | Returns query data asks the members of the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsMembers' ('ListNameParam' "thimura/haskell")
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

data ListsMembersCreate
-- | Returns the post parameter which adds a member to a list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsMembersCreate' ('ListNameParam' "thimura/haskell") ('ScreenNameParam' "thimura")
-- @
--
-- >>> listsMembersCreate (ListNameParam "thimura/haskell") (ScreenNameParam "thimura")
-- APIRequestPost "https://api.twitter.com/1.1/lists/members/create.json" [("slug","haskell"),("owner_screen_name","thimura"),("screen_name","thimura")]
-- >>> listsMembersCreate (ListIdParam 20849097) (UserIdParam 69179963)
-- APIRequestPost "https://api.twitter.com/1.1/lists/members/create.json" [("list_id","20849097"),("user_id","69179963")]
listsMembersCreate :: ListParam -> UserParam -> APIRequest ListsMembersCreate List
listsMembersCreate list user = APIRequestPost (endpoint ++ "lists/members/create.json") (mkListParam list ++ mkUserParam user)

data ListsDestroy
-- | Returns the post parameter which deletes the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsDestroy' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsDestroy (ListNameParam "thimura/haskell")
-- APIRequestPost "https://api.twitter.com/1.1/lists/destroy.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsDestroy (ListIdParam 20849097)
-- APIRequestPost "https://api.twitter.com/1.1/lists/destroy.json" [("list_id","20849097")]
listsDestroy :: ListParam -> APIRequest ListsDestroy List
listsDestroy list = APIRequestPost (endpoint ++ "lists/destroy.json") (mkListParam list)

data ListsUpdate
-- | Returns the post parameter which updates the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsUpdate' ('ListNameParam' "thimura/haskell") True (Just "Haskellers")
-- @
--
-- >>> listsUpdate (ListNameParam "thimura/haskell") True (Just "Haskellers")
-- APIRequestPost "https://api.twitter.com/1.1/lists/update.json" [("slug","haskell"),("owner_screen_name","thimura"),("description","Haskellers"),("mode","public")]
listsUpdate :: ListParam
            -> Bool -- ^ is public
            -> Maybe T.Text -- ^ description
            -> APIRequest ListsUpdate List
listsUpdate list isPublic description = APIRequestPost (endpoint ++ "lists/update.json") (mkListParam list ++ p')
  where
    p = [("mode", PVString . mode $ isPublic)]
    p' = maybe id (\d -> (("description", PVString d):)) description p
    mode True = "public"
    mode False = "private"

data ListsCreate
-- | Returns the post parameter which creates a new list for the authenticated user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsCreate' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsCreate "haskell" True Nothing
-- APIRequestPost "https://api.twitter.com/1.1/lists/create.json" [("name","haskell"),("mode","public")]
-- >>> listsCreate "haskell" False Nothing
-- APIRequestPost "https://api.twitter.com/1.1/lists/create.json" [("name","haskell"),("mode","private")]
-- >>> listsCreate "haskell" True (Just "Haskellers")
-- APIRequestPost "https://api.twitter.com/1.1/lists/create.json" [("description","Haskellers"),("name","haskell"),("mode","public")]
listsCreate :: T.Text -- ^ list name
            -> Bool -- ^ whether public(True) or private(False)
            -> Maybe T.Text -- ^ the description to give the list
            -> APIRequest ListsCreate List
listsCreate name isPublic description = APIRequestPost (endpoint ++ "lists/create.json") p'
  where
    p = [("name", PVString name), ("mode", PVString . mode $ isPublic)]
    p' = maybe id (\d -> (("description", PVString d):)) description p
    mode True = "public"
    mode False = "private"

data ListsShow
-- | Returns the request parameter which asks the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsShow' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsShow (ListNameParam "thimura/haskell")
-- APIRequestGet "https://api.twitter.com/1.1/lists/show.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsShow (ListIdParam 20849097)
-- APIRequestGet "https://api.twitter.com/1.1/lists/show.json" [("list_id","20849097")]
listsShow :: ListParam -> APIRequest ListsShow List
listsShow q = APIRequestGet (endpoint ++ "lists/show.json") (mkListParam q)

data MediaUpload
-- | Upload media and returns the media data.
--
-- You can update your status with multiple media by calling 'mediaUpload' and 'update' successively.
--
-- First, you should upload media with 'mediaUpload':
--
-- @
-- res1 <- 'call' twInfo mgr '$' 'mediaUpload' ('MediaFromFile' \"\/path\/to\/upload\/file1.png\")
-- res2 <- 'call' twInfo mgr '$' 'mediaUpload' ('MediaRequestBody' \"file2.png\" \"[.. file body ..]\")
-- @
--
-- and then collect the resulting media IDs and update your status by calling 'update':
--
-- @
-- 'call' twInfo mgr '$' 'update' \"Hello World\" '&' 'mediaIds' '?~' ['mediaId' res1, 'mediaId' res2]
-- @
--
-- See: <https://dev.twitter.com/docs/api/multiple-media-extended-entities>
--
-- >>> mediaUpload (MediaFromFile "/home/test/test.png")
-- APIRequestPostMultipart "https://upload.twitter.com/1.1/media/upload.json" []
mediaUpload :: MediaData
            -> APIRequest MediaUpload UploadedMedia
mediaUpload mediaData =
    APIRequestPostMultipart uri [] [mediaBody mediaData]
  where
    uri = "https://upload.twitter.com/1.1/media/upload.json"
    mediaBody (MediaFromFile fp) = partFileSource "media" fp
    mediaBody (MediaRequestBody filename filebody) = partFileRequestBody "media" filename filebody
