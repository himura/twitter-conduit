{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Web.Twitter.Conduit.Api (
    -- * Status
    StatusesMentionsTimeline,
    statusesMentionsTimeline,
    StatusesUserTimeline,
    statusesUserTimeline,
    StatusesHomeTimeline,
    statusesHomeTimeline,
    StatusesRetweetsOfMe,
    statusesRetweetsOfMe,
    StatusesRetweetsId,
    statusesRetweetsId,
    StatusesShowId,
    statusesShowId,
    StatusesDestroyId,
    statusesDestroyId,
    StatusesUpdate,
    statusesUpdate,
    StatusesRetweetId,
    statusesRetweetId,
    StatusesUpdateWithMedia,
    statusesUpdateWithMedia,
    StatusesLookup,
    statusesLookup,

    -- * Search
    SearchTweets,
    searchTweets,
    search,

    -- * Direct Messages
    DirectMessages,
    directMessages,
    DirectMessagesSent,
    directMessagesSent,
    DirectMessagesShow,
    directMessagesShow,
    DirectMessagesDestroy,
    directMessagesDestroy,
    DirectMessagesNew,
    directMessagesNew,

    -- * Friends & Followers
    FriendshipsNoRetweetsIds,
    friendshipsNoRetweetsIds,
    FriendsIds,
    friendsIds,
    FollowersIds,
    followersIds,
    FriendshipsIncoming,
    friendshipsIncoming,
    FriendshipsOutgoing,
    friendshipsOutgoing,
    FriendshipsCreate,
    friendshipsCreate,
    FriendshipsDestroy,
    friendshipsDestroy,
    -- , friendshipsUpdate
    -- , friendshipsShow
    FriendsList,
    friendsList,
    FollowersList,
    followersList,
    -- , friendshipsLookup

    -- * Users

    -- , accountSettings
    AccountVerifyCredentials,
    accountVerifyCredentials,
    -- , accountSettingsUpdate
    -- , accountUpdateDeliveryDevice
    AccountUpdateProfile,
    accountUpdateProfile,
    -- , accountUpdateProfileBackgroundImage
    -- , accountUpdateProfileColors
    -- , accoutUpdateProfileImage
    -- , blocksList
    -- , blocksIds
    -- , blocksCreate
    -- , blocksDestroy

    UsersLookup,
    usersLookup,
    UsersShow,
    usersShow,
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
    FavoritesList,
    favoritesList,
    FavoritesDestroy,
    favoritesDestroy,
    FavoritesCreate,
    favoritesCreate,

    -- * Lists

    -- , listsList
    ListsStatuses,
    listsStatuses,
    ListsMembersDestroy,
    listsMembersDestroy,
    ListsMemberships,
    listsMemberships,
    ListsSubscribers,
    listsSubscribers,
    -- , listsSubscribersCreate
    -- , listsSubscribersShow
    -- , listsSubscribersDestroy
    ListsMembersCreateAll,
    listsMembersCreateAll,
    -- , listsMembersShow
    ListsMembers,
    listsMembers,
    ListsMembersCreate,
    listsMembersCreate,
    ListsDestroy,
    listsDestroy,
    ListsUpdate,
    listsUpdate,
    ListsCreate,
    listsCreate,
    ListsShow,
    listsShow,
    ListsSubscriptions,
    listsSubscriptions,
    ListsMembersDestroyAll,
    listsMembersDestroyAll,
    ListsOwnerships,
    listsOwnerships,

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
    MediaUpload,
    mediaUpload,
) where

import Data.Aeson
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Network.HTTP.Client.MultipartFormData (partFileRequestBody, partFileSource)
import Web.Twitter.Conduit.Cursor (EventsCursorKey, IdsCursorKey, ListsCursorKey, UsersCursorKey, WithCursor)
import Web.Twitter.Conduit.Internal.APIRequest (
    APIRequest (APIRequest),
    BodyEmpty,
    BodyJSON (..),
    BodyMultipart (..),
    EmptyParams,
    Method (..),
    PV (..),
    Param (..),
 )
import Web.Twitter.Conduit.Internal.APIResponse (NoContent)
import Web.Twitter.Conduit.Parameters (
    ListParam,
    MediaData (..),
    TweetMode,
    UserListParam,
    UserParam,
    mkListParam,
    mkUserListParam,
    mkUserParam,
 )
import Web.Twitter.Types (
    DirectMessage,
    List,
    RetweetedStatus,
    SearchResult,
    Status,
    StatusId,
    URIString,
    UploadedMedia,
    User,
    UserId,
 )

endpointBase :: String
endpointBase = "https://api.twitter.com/1.1/"

-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedLabels
-- >>> import Control.Lens

-- * Timelines

-- | Returns query data asks the most recent mentions for the authenticating user.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' 'mentionsTimeline'
-- @
--
-- >>> statusesMentionsTimeline
-- APIRequest GET "https://api.twitter.com/1.1/statuses/mentions_timeline.json" []
statusesMentionsTimeline :: APIRequest StatusesMentionsTimeline BodyEmpty [Status]
statusesMentionsTimeline = APIRequest GET (endpointBase ++ "statuses/mentions_timeline.json") [] ()

type StatusesMentionsTimeline =
    '[ "count" ':= Integer
     , "since_id" ':= Integer
     , "max_id" ':= Integer
     , "trim_user" ':= Bool
     , "contributor_details" ':= Bool
     , "include_entities" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns query data asks a collection of the most recent Tweets posted by the user indicated by the screen_name or user_id parameters.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' $ 'userTimeline' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> statusesUserTimeline (ScreenNameParam "thimura")
-- APIRequest GET "https://api.twitter.com/1.1/statuses/user_timeline.json" [("screen_name","thimura")]
-- >>> statusesUserTimeline (ScreenNameParam "thimura") & #include_rts ?~ True & #count ?~ 200
-- APIRequest GET "https://api.twitter.com/1.1/statuses/user_timeline.json" [("count","200"),("include_rts","true"),("screen_name","thimura")]
statusesUserTimeline :: UserParam -> APIRequest StatusesUserTimeline BodyEmpty [Status]
statusesUserTimeline q = APIRequest GET (endpointBase ++ "statuses/user_timeline.json") (mkUserParam q) ()

type StatusesUserTimeline =
    '[ "count" ':= Integer
     , "since_id" ':= Integer
     , "max_id" ':= Integer
     , "trim_user" ':= Bool
     , "exclude_replies" ':= Bool
     , "contributor_details" ':= Bool
     , "include_rts" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns query data asks a collection of the most recentTweets and retweets posted by the authenticating user and the users they follow.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' 'homeTimeline'
-- @
--
-- >>> statusesHomeTimeline
-- APIRequest GET "https://api.twitter.com/1.1/statuses/home_timeline.json" []
-- >>> statusesHomeTimeline & #count ?~ 200
-- APIRequest GET "https://api.twitter.com/1.1/statuses/home_timeline.json" [("count","200")]
statusesHomeTimeline :: APIRequest StatusesHomeTimeline BodyEmpty [Status]
statusesHomeTimeline = APIRequest GET (endpointBase ++ "statuses/home_timeline.json") [] ()

type StatusesHomeTimeline =
    '[ "count" ':= Integer
     , "since_id" ':= Integer
     , "max_id" ':= Integer
     , "trim_user" ':= Bool
     , "exclude_replies" ':= Bool
     , "contributor_details" ':= Bool
     , "include_entities" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns query data asks the most recent tweets authored by the authenticating user that have been retweeted by others.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' 'retweetsOfMe'
-- @
--
-- >>> statusesRetweetsOfMe
-- APIRequest GET "https://api.twitter.com/1.1/statuses/retweets_of_me.json" []
-- >>> statusesRetweetsOfMe & #count ?~ 100
-- APIRequest GET "https://api.twitter.com/1.1/statuses/retweets_of_me.json" [("count","100")]
statusesRetweetsOfMe :: APIRequest StatusesRetweetsOfMe BodyEmpty [Status]
statusesRetweetsOfMe = APIRequest GET (endpointBase ++ "statuses/retweets_of_me.json") [] ()

type StatusesRetweetsOfMe =
    '[ "count" ':= Integer
     , "since_id" ':= Integer
     , "max_id" ':= Integer
     , "trim_user" ':= Bool
     , "include_entities" ':= Bool
     , "include_user_entities" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- * Tweets

-- | Returns query data that asks for the most recent retweets of the specified tweet
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'retweetsId' 1234567890
-- @
--
-- >>> statusesRetweetsId 1234567890
-- APIRequest GET "https://api.twitter.com/1.1/statuses/retweets/1234567890.json" []
-- >>> statusesRetweetsId 1234567890 & #count ?~ 100
-- APIRequest GET "https://api.twitter.com/1.1/statuses/retweets/1234567890.json" [("count","100")]
statusesRetweetsId :: StatusId -> APIRequest StatusesRetweetsId BodyEmpty [RetweetedStatus]
statusesRetweetsId status_id = APIRequest GET uri [] ()
  where
    uri = endpointBase ++ "statuses/retweets/" ++ show status_id ++ ".json"

type StatusesRetweetsId =
    '[ "count" ':= Integer
     , "trim_user" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns query data asks a single Tweet, specified by the id parameter.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'showId' 1234567890
-- @
--
-- >>> statusesShowId 1234567890
-- APIRequest GET "https://api.twitter.com/1.1/statuses/show/1234567890.json" []
-- >>> statusesShowId 1234567890 & #include_my_retweet ?~ True
-- APIRequest GET "https://api.twitter.com/1.1/statuses/show/1234567890.json" [("include_my_retweet","true")]
statusesShowId :: StatusId -> APIRequest StatusesShowId BodyEmpty Status
statusesShowId status_id = APIRequest GET uri [] ()
  where
    uri = endpointBase ++ "statuses/show/" ++ show status_id ++ ".json"

type StatusesShowId =
    '[ "trim_user" ':= Bool
     , "include_my_retweet" ':= Bool
     , "include_entities" ':= Bool
     , "include_ext_alt_text" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns post data which destroys the status specified by the require ID parameter.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'destroyId' 1234567890
-- @
--
-- >>> statusesDestroyId 1234567890
-- APIRequest POST "https://api.twitter.com/1.1/statuses/destroy/1234567890.json" []
statusesDestroyId :: StatusId -> APIRequest StatusesDestroyId BodyEmpty Status
statusesDestroyId status_id = APIRequest POST uri [] ()
  where
    uri = endpointBase ++ "statuses/destroy/" ++ show status_id ++ ".json"

type StatusesDestroyId =
    '[ "trim_user" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns post data which updates the authenticating user's current status.
-- To upload an image to accompany the tweet, use 'updateWithMedia'.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'update' \"Hello World\"
-- @
--
-- >>> statusesUpdate "Hello World"
-- APIRequest POST "https://api.twitter.com/1.1/statuses/update.json" [("status","Hello World")]
-- >>> statusesUpdate "Hello World" & #in_reply_to_status_id ?~ 1234567890
-- APIRequest POST "https://api.twitter.com/1.1/statuses/update.json" [("in_reply_to_status_id","1234567890"),("status","Hello World")]
statusesUpdate :: T.Text -> APIRequest StatusesUpdate BodyEmpty Status
statusesUpdate status = APIRequest POST uri [("status", PVString status)] ()
  where
    uri = endpointBase ++ "statuses/update.json"

type StatusesUpdate =
    '[ "in_reply_to_status_id" ':= Integer
     , -- , "lat_long"
       -- , "place_id"
       "display_coordinates" ':= Bool
     , "trim_user" ':= Bool
     , "media_ids" ':= [Integer]
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns post data which retweets a tweet, specified by ID.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'retweetId' 1234567890
-- @
--
-- >>> statusesRetweetId 1234567890
-- APIRequest POST "https://api.twitter.com/1.1/statuses/retweet/1234567890.json" []
statusesRetweetId :: StatusId -> APIRequest StatusesRetweetId BodyEmpty RetweetedStatus
statusesRetweetId status_id = APIRequest POST uri [] ()
  where
    uri = endpointBase ++ "statuses/retweet/" ++ show status_id ++ ".json"

type StatusesRetweetId =
    '[ "trim_user" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns post data which updates the authenticating user's current status and attaches media for upload.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'updateWithMedia' \"Hello World\" ('MediaFromFile' \"/home/thimura/test.jpeg\")
-- @
--
-- >>> statusesUpdateWithMedia "Hello World" (MediaFromFile "/home/fuga/test.jpeg")
-- APIRequestMultipart POST "https://api.twitter.com/1.1/statuses/update_with_media.json" [("status","Hello World")]
statusesUpdateWithMedia ::
    T.Text ->
    MediaData ->
    APIRequest StatusesUpdateWithMedia BodyMultipart Status
statusesUpdateWithMedia tweet mediaData =
    APIRequest POST uri [("status", PVString tweet)] (BodyMultipart [mediaBody mediaData])
  where
    uri = endpointBase ++ "statuses/update_with_media.json"
    mediaBody (MediaFromFile fp) = partFileSource "media[]" fp
    mediaBody (MediaRequestBody filename filebody) = partFileRequestBody "media[]" filename filebody

type StatusesUpdateWithMedia =
    '[ "possibly_sensitive" ':= Bool
     , "in_reply_to_status_id" ':= Integer
     , -- , "lat_long"
       -- , "place_id"
       "display_coordinates" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns fully-hydrated tweet objects for up to 100 tweets per request, as specified by comma-separated values passed to the id parameter.
--
-- You can perform a request using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'lookup' [20, 432656548536401920]
-- @
--
-- >>> statusesLookup [10]
-- APIRequest GET "https://api.twitter.com/1.1/statuses/lookup.json" [("id","10")]
-- >>> statusesLookup [10, 432656548536401920]
-- APIRequest GET "https://api.twitter.com/1.1/statuses/lookup.json" [("id","10,432656548536401920")]
-- >>> statusesLookup [10, 432656548536401920] & #include_entities ?~ True
-- APIRequest GET "https://api.twitter.com/1.1/statuses/lookup.json" [("include_entities","true"),("id","10,432656548536401920")]
statusesLookup :: [StatusId] -> APIRequest StatusesLookup BodyEmpty [Status]
statusesLookup ids = APIRequest GET (endpointBase ++ "statuses/lookup.json") [("id", PVIntegerArray ids)] ()

type StatusesLookup =
    '[ "include_entities" ':= Bool
     , "trim_user" ':= Bool
     , "map" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns search query.
--
-- You can perform a search query using 'call':
--
-- @
-- res <- 'call' ('searchTweets' \"search text\")
-- 'print' $ res ^. 'searchResultStatuses'
-- @
--
-- >>> searchTweets "search text"
-- APIRequest GET "https://api.twitter.com/1.1/search/tweets.json" [("q","search text")]
-- >>> searchTweets "search text" & #lang ?~ "ja" & #count ?~ 100
-- APIRequest GET "https://api.twitter.com/1.1/search/tweets.json" [("count","100"),("lang","ja"),("q","search text")]
searchTweets ::
    -- | search string
    T.Text ->
    APIRequest SearchTweets BodyEmpty (SearchResult [Status])
searchTweets q = APIRequest GET (endpointBase ++ "search/tweets.json") [("q", PVString q)] ()

type SearchTweets =
    '[ "lang" ':= T.Text
     , "locale" ':= T.Text
     , "result_type" ':= T.Text
     , "count" ':= Integer
     , "until" ':= Day
     , "since_id" ':= Integer
     , "max_id" ':= Integer
     , "include_entities" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Alias of 'searchTweets', for backward compatibility
search ::
    -- | search string
    T.Text ->
    APIRequest SearchTweets BodyEmpty (SearchResult [Status])
search = searchTweets
{-# DEPRECATED search "Please use Web.Twitter.Conduit.searchTweets" #-}

-- | Returns query data which asks recent direct messages sent to the authenticating user.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'directMessages' '&' #count '?~' 50
-- @
--
-- >>> directMessages
-- APIRequest GET "https://api.twitter.com/1.1/direct_messages/events/list.json" []
-- >>> directMessages & #count ?~ 50
-- APIRequest GET "https://api.twitter.com/1.1/direct_messages/events/list.json" [("count","50")]
directMessages :: APIRequest DirectMessages BodyEmpty (WithCursor T.Text EventsCursorKey DirectMessage)
directMessages = APIRequest GET (endpointBase ++ "direct_messages/events/list.json") [] ()

type DirectMessages =
    '[ "count" ':= Integer
     , "include_entities" ':= Bool
     , "skip_status" ':= Bool
     , "full_text" ':= Bool
     , "cursor" ':= T.Text
     ]

-- | Returns query data which asks recent direct messages sent by the authenticating user.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'directMessagesSent' '&' #count '?~' 100
-- @
--
-- >>> directMessagesSent
-- APIRequest GET "https://api.twitter.com/1.1/direct_messages/sent.json" []
-- >>> directMessagesSent & #count ?~ 100
-- APIRequest GET "https://api.twitter.com/1.1/direct_messages/sent.json" [("count","100")]
directMessagesSent :: APIRequest DirectMessagesSent BodyEmpty [DirectMessage]
directMessagesSent = APIRequest GET (endpointBase ++ "direct_messages/sent.json") [] ()

type DirectMessagesSent =
    '[ "since_id" ':= Integer
     , "max_id" ':= Integer
     , "count" ':= Integer
     , "include_entities" ':= Bool
     , "page" ':= Integer
     , "skip_status" ':= Bool
     , "full_text" ':= Bool
     ]

-- | Returns query data which asks a single direct message, specified by an id parameter.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'directMessagesShow' 1234567890
-- @
--
-- >>> directMessagesShow 1234567890
-- APIRequest GET "https://api.twitter.com/1.1/direct_messages/show.json" [("id","1234567890")]
directMessagesShow :: StatusId -> APIRequest DirectMessagesShow BodyEmpty DirectMessage
directMessagesShow sId = APIRequest GET (endpointBase ++ "direct_messages/show.json") [("id", PVInteger sId)] ()

type DirectMessagesShow =
    '[ "full_text" ':= Bool
     ]

-- | Returns post data which destroys the direct message specified in the required ID parameter.
--
-- You can perform a query using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'directMessagesDestroy' 1234567890
-- @
--
-- >>> directMessagesDestroy 1234567890
-- APIRequest DELETE "https://api.twitter.com/1.1/direct_messages/events/destroy.json" [("id","1234567890")]
directMessagesDestroy :: StatusId -> APIRequest DirectMessagesDestroy BodyEmpty NoContent
directMessagesDestroy sId = APIRequest DELETE (endpointBase ++ "direct_messages/events/destroy.json") [("id", PVInteger sId)] ()

type DirectMessagesDestroy = EmptyParams

newtype DirectMessagesNewResponse = DirectMessagesNewResponse
    { directMessageBody :: DirectMessage
    }
    deriving (Show, Eq)

instance FromJSON DirectMessagesNewResponse where
    parseJSON = withObject "DirectMessagesNewResponse" $ \o -> DirectMessagesNewResponse <$> o .: "event"

-- | Returns post data which sends a new direct message to the specified user from the authenticating user.
--
-- You can perform a post using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'directMessagesNew' (ScreenNameParam \"thimura\") \"Hello DM\"
-- @
--
-- >>> directMessagesNew 69179963 "Hello thimura! by UserId"
-- APIRequestJSON POST "https://api.twitter.com/1.1/direct_messages/events/new.json" []
directMessagesNew :: RecipientId -> T.Text -> APIRequest DirectMessagesNew (BodyJSON Value) DirectMessagesNewResponse
directMessagesNew up msg =
    APIRequest POST (endpointBase ++ "direct_messages/events/new.json") [] (BodyJSON body)
  where
    body =
        object
            [ "event"
                .= object
                    [ "type" .= ("message_create" :: String)
                    , "message_create"
                        .= object
                            [ "target" .= object ["recipient_id" .= up]
                            , "message_data" .= object ["text" .= msg]
                            ]
                    ]
            ]

type DirectMessagesNew = EmptyParams

type RecipientId = Integer

-- | Returns a collection of user_ids that the currently authenticated user does not want to receive retweets from.
--
-- You can perform a request using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendshipsNoRetweetsIds'
-- @
--
-- >>> friendshipsNoRetweetsIds
-- APIRequest GET "https://api.twitter.com/1.1/friendships/no_retweets/ids.json" []
friendshipsNoRetweetsIds :: APIRequest FriendshipsNoRetweetsIds BodyEmpty [UserId]
friendshipsNoRetweetsIds = APIRequest GET (endpointBase ++ "friendships/no_retweets/ids.json") [] ()

type FriendshipsNoRetweetsIds = EmptyParams

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
-- APIRequest GET "https://api.twitter.com/1.1/friends/ids.json" [("screen_name","thimura")]
-- >>> friendsIds (ScreenNameParam "thimura") & #count ?~ 5000
-- APIRequest GET "https://api.twitter.com/1.1/friends/ids.json" [("count","5000"),("screen_name","thimura")]
friendsIds :: UserParam -> APIRequest FriendsIds BodyEmpty (WithCursor Integer IdsCursorKey UserId)
friendsIds q = APIRequest GET (endpointBase ++ "friends/ids.json") (mkUserParam q) ()

type FriendsIds =
    '[ "count" ':= Integer
     , "cursor" ':= Integer
     ]

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
-- APIRequest GET "https://api.twitter.com/1.1/followers/ids.json" [("screen_name","thimura")]
-- >>> followersIds (ScreenNameParam "thimura") & #count ?~ 5000
-- APIRequest GET "https://api.twitter.com/1.1/followers/ids.json" [("count","5000"),("screen_name","thimura")]
followersIds :: UserParam -> APIRequest FollowersIds BodyEmpty (WithCursor Integer IdsCursorKey UserId)
followersIds q = APIRequest GET (endpointBase ++ "followers/ids.json") (mkUserParam q) ()

type FollowersIds =
    '[ "count" ':= Integer
     , "cursor" ':= Integer
     ]

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
-- APIRequest GET "https://api.twitter.com/1.1/friendships/incoming.json" []
friendshipsIncoming :: APIRequest FriendshipsIncoming BodyEmpty (WithCursor Integer IdsCursorKey UserId)
friendshipsIncoming = APIRequest GET (endpointBase ++ "friendships/incoming.json") [] ()

type FriendshipsIncoming =
    '[ "cursor" ':= Integer
     ]

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
-- APIRequest GET "https://api.twitter.com/1.1/friendships/outgoing.json" []
friendshipsOutgoing :: APIRequest FriendshipsOutgoing BodyEmpty (WithCursor Integer IdsCursorKey UserId)
friendshipsOutgoing = APIRequest GET (endpointBase ++ "friendships/outgoing.json") [] ()

type FriendshipsOutgoing =
    '[ "cursor" ':= Integer
     ]

-- | Returns post data which follows the user specified in the ID parameter.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendshipsCreate' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> friendshipsCreate (ScreenNameParam "thimura")
-- APIRequest POST "https://api.twitter.com/1.1/friendships/create.json" [("screen_name","thimura")]
-- >>> friendshipsCreate (UserIdParam 69179963)
-- APIRequest POST "https://api.twitter.com/1.1/friendships/create.json" [("user_id","69179963")]
friendshipsCreate :: UserParam -> APIRequest FriendshipsCreate BodyEmpty User
friendshipsCreate user = APIRequest POST (endpointBase ++ "friendships/create.json") (mkUserParam user) ()

type FriendshipsCreate =
    '[ "follow" ':= Bool
     ]

-- | Returns post data which unfollows the user specified in the ID parameter.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'friendshipsDestroy' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> friendshipsDestroy (ScreenNameParam "thimura")
-- APIRequest POST "https://api.twitter.com/1.1/friendships/destroy.json" [("screen_name","thimura")]
-- >>> friendshipsDestroy (UserIdParam 69179963)
-- APIRequest POST "https://api.twitter.com/1.1/friendships/destroy.json" [("user_id","69179963")]
friendshipsDestroy :: UserParam -> APIRequest FriendshipsDestroy BodyEmpty User
friendshipsDestroy user = APIRequest POST (endpointBase ++ "friendships/destroy.json") (mkUserParam user) ()

type FriendshipsDestroy = EmptyParams

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
-- APIRequest GET "https://api.twitter.com/1.1/friends/list.json" [("screen_name","thimura")]
-- >>> friendsList (UserIdParam 69179963)
-- APIRequest GET "https://api.twitter.com/1.1/friends/list.json" [("user_id","69179963")]
friendsList :: UserParam -> APIRequest FriendsList BodyEmpty (WithCursor Integer UsersCursorKey User)
friendsList q = APIRequest GET (endpointBase ++ "friends/list.json") (mkUserParam q) ()

type FriendsList =
    '[ "count" ':= Integer
     , "cursor" ':= Integer
     , "skip_status" ':= Bool
     , "include_user_entities" ':= Bool
     ]

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
-- APIRequest GET "https://api.twitter.com/1.1/followers/list.json" [("screen_name","thimura")]
-- >>> followersList (UserIdParam 69179963)
-- APIRequest GET "https://api.twitter.com/1.1/followers/list.json" [("user_id","69179963")]
followersList :: UserParam -> APIRequest FollowersList BodyEmpty (WithCursor Integer UsersCursorKey User)
followersList q = APIRequest GET (endpointBase ++ "followers/list.json") (mkUserParam q) ()

type FollowersList =
    '[ "count" ':= Integer
     , "cursor" ':= Integer
     , "skip_status" ':= Bool
     , "include_user_entities" ':= Bool
     ]

-- | Returns query data asks that the credential is valid.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'accountVerifyCredentials'
-- @
--
-- >>> accountVerifyCredentials
-- APIRequest GET "https://api.twitter.com/1.1/account/verify_credentials.json" []
accountVerifyCredentials :: APIRequest AccountVerifyCredentials BodyEmpty User
accountVerifyCredentials = APIRequest GET (endpointBase ++ "account/verify_credentials.json") [] ()

type AccountVerifyCredentials =
    '[ "include_entities" ':= Bool
     , "skip_status" ':= Bool
     , "include_email" ':= Bool
     ]

-- | Returns user object with updated fields.
-- Note that while no specific parameter is required, you need to provide at least one parameter before executing the query.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'accountUpdateProfile' & #url ?~ \"http://www.example.com\"
-- @
--
-- >>> accountUpdateProfile & #url ?~ "http://www.example.com"
-- APIRequest POST "https://api.twitter.com/1.1/account/update_profile.json" [("url","http://www.example.com")]
accountUpdateProfile :: APIRequest AccountUpdateProfile BodyEmpty User
accountUpdateProfile = APIRequest POST (endpointBase ++ "account/update_profile.json") [] ()

type AccountUpdateProfile =
    '[ "include_entities" ':= Bool
     , "skip_status" ':= Bool
     , "name" ':= T.Text
     , "url" ':= URIString
     , "location" ':= T.Text
     , "description" ':= T.Text
     , "profile_link_color" ':= T.Text
     ]

-- | Returns query data asks user objects.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'usersLookup' ('ScreenNameListParam' [\"thimura\", \"twitterapi\"])
-- @
--
-- >>> usersLookup (ScreenNameListParam ["thimura", "twitterapi"])
-- APIRequest GET "https://api.twitter.com/1.1/users/lookup.json" [("screen_name","thimura,twitterapi")]
usersLookup :: UserListParam -> APIRequest UsersLookup BodyEmpty [User]
usersLookup q = APIRequest GET (endpointBase ++ "users/lookup.json") (mkUserListParam q) ()

type UsersLookup =
    '[ "include_entities" ':= Bool
     ]

-- | Returns query data asks the user specified by user id or screen name parameter.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'usersShow' ('ScreenNameParam' \"thimura\")
-- @
--
-- >>> usersShow (ScreenNameParam "thimura")
-- APIRequest GET "https://api.twitter.com/1.1/users/show.json" [("screen_name","thimura")]
usersShow :: UserParam -> APIRequest UsersShow BodyEmpty User
usersShow q = APIRequest GET (endpointBase ++ "users/show.json") (mkUserParam q) ()

type UsersShow =
    '[ "include_entities" ':= Bool
     ]

-- | Returns the 20 most recent Tweets favorited by the specified user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'favoritesList' (ScreenNameParam \"thimura\")
-- @
--
-- >>> favoritesList Nothing
-- APIRequest GET "https://api.twitter.com/1.1/favorites/list.json" []
-- >>> favoritesList (Just (ScreenNameParam "thimura"))
-- APIRequest GET "https://api.twitter.com/1.1/favorites/list.json" [("screen_name","thimura")]
-- >>> favoritesList (Just (UserIdParam 69179963))
-- APIRequest GET "https://api.twitter.com/1.1/favorites/list.json" [("user_id","69179963")]
favoritesList :: Maybe UserParam -> APIRequest FavoritesList BodyEmpty [Status]
favoritesList mbuser = APIRequest GET (endpointBase ++ "favorites/list.json") (mkParam mbuser) ()
  where
    mkParam Nothing = []
    mkParam (Just usr) = mkUserParam usr

type FavoritesList =
    '[ "count" ':= Integer
     , "since_id" ':= Integer
     , "max_id" ':= Integer
     , "include_entities" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns post data which favorites the status specified in the ID parameter as the authenticating user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'favoritesCreate' 1234567890
-- @
--
-- >>> favoritesCreate 1234567890
-- APIRequest POST "https://api.twitter.com/1.1/favorites/create.json" [("id","1234567890")]
favoritesCreate :: StatusId -> APIRequest FavoritesCreate BodyEmpty Status
favoritesCreate sid = APIRequest POST (endpointBase ++ "favorites/create.json") [("id", PVInteger sid)] ()

type FavoritesCreate =
    '[ "include_entities" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns post data unfavorites the status specified in the ID parameter as the authenticating user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'favoritesDestroy' 1234567890
-- @
--
-- >>> favoritesDestroy 1234567890
-- APIRequest POST "https://api.twitter.com/1.1/favorites/destroy.json" [("id","1234567890")]
favoritesDestroy :: StatusId -> APIRequest FavoritesDestroy BodyEmpty Status
favoritesDestroy sid = APIRequest POST (endpointBase ++ "favorites/destroy.json") [("id", PVInteger sid)] ()

type FavoritesDestroy =
    '[ "include_entities" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

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
-- res <- sourceWithMaxId ('listsStatuses' ('ListNameParam' "thimura/haskell") & #count ?~ 200) $$ CL.take 1000
-- @
--
-- >>> listsStatuses (ListNameParam "thimura/haskell")
-- APIRequest GET "https://api.twitter.com/1.1/lists/statuses.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsStatuses (ListIdParam 20849097)
-- APIRequest GET "https://api.twitter.com/1.1/lists/statuses.json" [("list_id","20849097")]
listsStatuses :: ListParam -> APIRequest ListsStatuses BodyEmpty [Status]
listsStatuses q = APIRequest GET (endpointBase ++ "lists/statuses.json") (mkListParam q) ()

type ListsStatuses =
    '[ "since_id" ':= Integer
     , "max_id" ':= Integer
     , "count" ':= Integer
     , "include_entities" ':= Bool
     , "include_rts" ':= Bool
     , "tweet_mode" ':= TweetMode
     ]

-- | Returns the post parameter which removes the specified member from the list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsMembersDestroy' ('ListNameParam' "thimura/haskell") ('ScreenNameParam' "thimura")
-- @
--
-- >>> listsMembersDestroy (ListNameParam "thimura/haskell") (ScreenNameParam "thimura")
-- APIRequest POST "https://api.twitter.com/1.1/lists/members/destroy.json" [("slug","haskell"),("owner_screen_name","thimura"),("screen_name","thimura")]
-- >>> listsMembersDestroy (ListIdParam 20849097) (UserIdParam 69179963)
-- APIRequest POST "https://api.twitter.com/1.1/lists/members/destroy.json" [("list_id","20849097"),("user_id","69179963")]
listsMembersDestroy :: ListParam -> UserParam -> APIRequest ListsMembersDestroy BodyEmpty List
listsMembersDestroy list user = APIRequest POST (endpointBase ++ "lists/members/destroy.json") (mkListParam list ++ mkUserParam user) ()

type ListsMembersDestroy = EmptyParams

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
-- APIRequest GET "https://api.twitter.com/1.1/lists/memberships.json" []
-- >>> listsMemberships (Just (ScreenNameParam "thimura"))
-- APIRequest GET "https://api.twitter.com/1.1/lists/memberships.json" [("screen_name","thimura")]
-- >>> listsMemberships (Just (UserIdParam 69179963))
-- APIRequest GET "https://api.twitter.com/1.1/lists/memberships.json" [("user_id","69179963")]
listsMemberships :: Maybe UserParam -> APIRequest ListsMemberships BodyEmpty (WithCursor Integer ListsCursorKey List)
listsMemberships q = APIRequest GET (endpointBase ++ "lists/memberships.json") (maybe [] mkUserParam q) ()

type ListsMemberships =
    '[ "count" ':= Integer
     , "cursor" ':= Integer
     ]

-- | Returns the request parameter which asks the subscribers of the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsSubscribers' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsSubscribers (ListNameParam "thimura/haskell")
-- APIRequest GET "https://api.twitter.com/1.1/lists/subscribers.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsSubscribers (ListIdParam 20849097)
-- APIRequest GET "https://api.twitter.com/1.1/lists/subscribers.json" [("list_id","20849097")]
listsSubscribers :: ListParam -> APIRequest ListsSubscribers BodyEmpty (WithCursor Integer UsersCursorKey User)
listsSubscribers q = APIRequest GET (endpointBase ++ "lists/subscribers.json") (mkListParam q) ()

type ListsSubscribers =
    '[ "count" ':= Integer
     , "cursor" ':= Integer
     , "skip_status" ':= Bool
     ]

-- | Returns the request parameter which obtains a collection of the lists the specified user is subscribed to.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsSubscriptions' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsSubscriptions Nothing
-- APIRequest GET "https://api.twitter.com/1.1/lists/subscriptions.json" []
-- >>> listsSubscriptions (Just (ScreenNameParam "thimura"))
-- APIRequest GET "https://api.twitter.com/1.1/lists/subscriptions.json" [("screen_name","thimura")]
-- >>> listsSubscriptions (Just (UserIdParam 69179963))
-- APIRequest GET "https://api.twitter.com/1.1/lists/subscriptions.json" [("user_id","69179963")]
listsSubscriptions :: Maybe UserParam -> APIRequest ListsSubscriptions BodyEmpty (WithCursor Integer ListsCursorKey List)
listsSubscriptions q = APIRequest GET (endpointBase ++ "lists/subscriptions.json") (maybe [] mkUserParam q) ()

type ListsSubscriptions =
    '[ "count" ':= Integer
     , "cursor" ':= Integer
     ]

-- | Returns the request parameter which asks the lists owned by the specified Twitter user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsOwnerships' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsOwnerships Nothing
-- APIRequest GET "https://api.twitter.com/1.1/lists/ownerships.json" []
-- >>> listsOwnerships (Just (ScreenNameParam "thimura"))
-- APIRequest GET "https://api.twitter.com/1.1/lists/ownerships.json" [("screen_name","thimura")]
-- >>> listsOwnerships (Just (UserIdParam 69179963))
-- APIRequest GET "https://api.twitter.com/1.1/lists/ownerships.json" [("user_id","69179963")]
listsOwnerships :: Maybe UserParam -> APIRequest ListsOwnerships BodyEmpty (WithCursor Integer ListsCursorKey List)
listsOwnerships q = APIRequest GET (endpointBase ++ "lists/ownerships.json") (maybe [] mkUserParam q) ()

type ListsOwnerships =
    '[ "count" ':= Integer
     , "cursor" ':= Integer
     ]

-- | Adds multiple members to a list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsMembersCreateAll' ('ListNameParam' "thimura/haskell") ('ScreenNameListParam' [\"thimura\", \"twitterapi\"])
-- @
--
-- >>> listsMembersCreateAll (ListNameParam "thimura/haskell") (ScreenNameListParam ["thimura", "twitterapi"])
-- APIRequest POST "https://api.twitter.com/1.1/lists/members/create_all.json" [("slug","haskell"),("owner_screen_name","thimura"),("screen_name","thimura,twitterapi")]
-- >>> listsMembersCreateAll (ListIdParam 20849097) (UserIdListParam [69179963, 6253282])
-- APIRequest POST "https://api.twitter.com/1.1/lists/members/create_all.json" [("list_id","20849097"),("user_id","69179963,6253282")]
listsMembersCreateAll :: ListParam -> UserListParam -> APIRequest ListsMembersCreateAll BodyEmpty List
listsMembersCreateAll list users = APIRequest POST (endpointBase ++ "lists/members/create_all.json") (mkListParam list ++ mkUserListParam users) ()

type ListsMembersCreateAll = EmptyParams

-- | Adds multiple members to a list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsMembersDestroyAll' ('ListNameParam' "thimura/haskell") ('ScreenNameListParam' [\"thimura\", \"twitterapi\"])
-- @
--
-- >>> listsMembersDestroyAll (ListNameParam "thimura/haskell") (ScreenNameListParam ["thimura", "twitterapi"])
-- APIRequest POST "https://api.twitter.com/1.1/lists/members/destroy_all.json" [("slug","haskell"),("owner_screen_name","thimura"),("screen_name","thimura,twitterapi")]
-- >>> listsMembersDestroyAll (ListIdParam 20849097) (UserIdListParam [69179963, 6253282])
-- APIRequest POST "https://api.twitter.com/1.1/lists/members/destroy_all.json" [("list_id","20849097"),("user_id","69179963,6253282")]
listsMembersDestroyAll :: ListParam -> UserListParam -> APIRequest ListsMembersDestroyAll BodyEmpty List
listsMembersDestroyAll list users = APIRequest POST (endpointBase ++ "lists/members/destroy_all.json") (mkListParam list ++ mkUserListParam users) ()

type ListsMembersDestroyAll = EmptyParams

-- | Returns query data asks the members of the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsMembers' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsMembers (ListNameParam "thimura/haskell")
-- APIRequest GET "https://api.twitter.com/1.1/lists/members.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsMembers (ListIdParam 20849097)
-- APIRequest GET "https://api.twitter.com/1.1/lists/members.json" [("list_id","20849097")]
listsMembers :: ListParam -> APIRequest ListsMembers BodyEmpty (WithCursor Integer UsersCursorKey User)
listsMembers q = APIRequest GET (endpointBase ++ "lists/members.json") (mkListParam q) ()

type ListsMembers =
    '[ "count" ':= Integer
     , "cursor" ':= Integer
     , "skip_status" ':= Bool
     ]

-- | Returns the post parameter which adds a member to a list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsMembersCreate' ('ListNameParam' "thimura/haskell") ('ScreenNameParam' "thimura")
-- @
--
-- >>> listsMembersCreate (ListNameParam "thimura/haskell") (ScreenNameParam "thimura")
-- APIRequest POST "https://api.twitter.com/1.1/lists/members/create.json" [("slug","haskell"),("owner_screen_name","thimura"),("screen_name","thimura")]
-- >>> listsMembersCreate (ListIdParam 20849097) (UserIdParam 69179963)
-- APIRequest POST "https://api.twitter.com/1.1/lists/members/create.json" [("list_id","20849097"),("user_id","69179963")]
listsMembersCreate :: ListParam -> UserParam -> APIRequest ListsMembersCreate BodyEmpty List
listsMembersCreate list user = APIRequest POST (endpointBase ++ "lists/members/create.json") (mkListParam list ++ mkUserParam user) ()

type ListsMembersCreate = EmptyParams

-- | Returns the post parameter which deletes the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsDestroy' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsDestroy (ListNameParam "thimura/haskell")
-- APIRequest POST "https://api.twitter.com/1.1/lists/destroy.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsDestroy (ListIdParam 20849097)
-- APIRequest POST "https://api.twitter.com/1.1/lists/destroy.json" [("list_id","20849097")]
listsDestroy :: ListParam -> APIRequest ListsDestroy BodyEmpty List
listsDestroy list = APIRequest POST (endpointBase ++ "lists/destroy.json") (mkListParam list) ()

type ListsDestroy = EmptyParams

-- | Returns the post parameter which updates the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsUpdate' ('ListNameParam' "thimura/haskell") True (Just "Haskellers")
-- @
--
-- >>> listsUpdate (ListNameParam "thimura/haskell") True (Just "Haskellers")
-- APIRequest POST "https://api.twitter.com/1.1/lists/update.json" [("slug","haskell"),("owner_screen_name","thimura"),("description","Haskellers"),("mode","public")]
listsUpdate ::
    ListParam ->
    -- | is public
    Bool ->
    -- | description
    Maybe T.Text ->
    APIRequest ListsUpdate BodyEmpty List
listsUpdate list isPublic description = APIRequest POST (endpointBase ++ "lists/update.json") (mkListParam list ++ p') ()
  where
    p = [("mode", PVString . mode $ isPublic)]
    p' = maybe id (\d -> (("description", PVString d) :)) description p
    mode True = "public"
    mode False = "private"

type ListsUpdate = EmptyParams

-- | Returns the post parameter which creates a new list for the authenticated user.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsCreate' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsCreate "haskell" True Nothing
-- APIRequest POST "https://api.twitter.com/1.1/lists/create.json" [("name","haskell"),("mode","public")]
-- >>> listsCreate "haskell" False Nothing
-- APIRequest POST "https://api.twitter.com/1.1/lists/create.json" [("name","haskell"),("mode","private")]
-- >>> listsCreate "haskell" True (Just "Haskellers")
-- APIRequest POST "https://api.twitter.com/1.1/lists/create.json" [("description","Haskellers"),("name","haskell"),("mode","public")]
listsCreate ::
    -- | list name
    T.Text ->
    -- | whether public(True) or private(False)
    Bool ->
    -- | the description to give the list
    Maybe T.Text ->
    APIRequest ListsCreate BodyEmpty List
listsCreate name isPublic description = APIRequest POST (endpointBase ++ "lists/create.json") p' ()
  where
    p = [("name", PVString name), ("mode", PVString . mode $ isPublic)]
    p' = maybe id (\d -> (("description", PVString d) :)) description p
    mode True = "public"
    mode False = "private"

type ListsCreate = EmptyParams

-- | Returns the request parameter which asks the specified list.
--
-- You can perform request by using 'call':
--
-- @
-- res <- 'call' twInfo mgr '$' 'listsShow' ('ListNameParam' "thimura/haskell")
-- @
--
-- >>> listsShow (ListNameParam "thimura/haskell")
-- APIRequest GET "https://api.twitter.com/1.1/lists/show.json" [("slug","haskell"),("owner_screen_name","thimura")]
-- >>> listsShow (ListIdParam 20849097)
-- APIRequest GET "https://api.twitter.com/1.1/lists/show.json" [("list_id","20849097")]
listsShow :: ListParam -> APIRequest ListsShow BodyEmpty List
listsShow q = APIRequest GET (endpointBase ++ "lists/show.json") (mkListParam q) ()

type ListsShow = EmptyParams

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
-- 'call' twInfo mgr '$' 'update' \"Hello World\" '&' #media_ids '?~' ['uploadedMediaId' res1, 'uploadedMediaId' res2]
-- @
--
-- See: <https://dev.twitter.com/docs/api/multiple-media-extended-entities>
--
-- >>> mediaUpload (MediaFromFile "/home/test/test.png")
-- APIRequestMultipart POST "https://upload.twitter.com/1.1/media/upload.json" []
mediaUpload ::
    MediaData ->
    APIRequest MediaUpload BodyMultipart UploadedMedia
mediaUpload mediaData =
    APIRequest POST uri [] (BodyMultipart [mediaBody mediaData])
  where
    uri = "https://upload.twitter.com/1.1/media/upload.json"
    mediaBody (MediaFromFile fp) = partFileSource "media" fp
    mediaBody (MediaRequestBody filename filebody) = partFileRequestBody "media" filename filebody

type MediaUpload = EmptyParams
