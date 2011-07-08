module Web.Twitter.Enumerator.Fetch
       ( api
       , statusesPublicTimeline
       , statusesUserTimeline
       , statusesFriendsTimeline
       , statusesReplies
       , statusesMentions
       , statusesRetweetedByMe
       , statusesRetweetedToMe
       , statusesRetweetsOfMe
       , friendsIds
       , followerIds
       , userstream
       )
       where

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Utils

import Web.Authenticate.OAuth

import Data.Aeson hiding (Error)

import Network.HTTP.Enumerator
import Data.Enumerator hiding (map, filter)
import qualified Data.Enumerator.List as EL

import Data.ByteString (ByteString)

import Control.Monad.State

api :: String -> [(ByteString, Maybe ByteString)] -> Iteratee ByteString IO a -> Manager -> TW (Iteratee ByteString IO a)
api url query iter mgr = do
  env <- get
  req <- liftIO $ signOAuth (twOAuth env) (twCredential env) =<< parseUrl url
  let req' = req { queryString = query, proxy = twProxy env }
  return $ http req (\_ _ -> iter) mgr

statuses :: String -> [(ByteString, Maybe ByteString)] -> Manager -> TW (Iteratee ByteString IO [Status])
statuses url query = api aurl query iter
  where iter = enumLine =$ enumJSON =$ skipNothing =$ enumJsonToStatus =$ EL.consume
        aurl = "https://api.twitter.com/1/statuses/" ++ url

statusesPublicTimeline = statuses "public_timeline.json"
statusesUserTimeline = statuses "user_timeline.json"
statusesFriendsTimeline = statuses "friends_timeline.json"
statusesReplies = statuses "replies.json"
statusesMentions = statuses "mentions.json"
statusesRetweetedByMe = statuses "retweeted_by_me.json"
statusesRetweetedToMe = statuses "retweeted_to_me.json"
statusesRetweetsOfMe = statuses "retweeted_of_me.json"

iterFold :: FromJSON a => Iteratee ByteString IO [a]
iterFold = enumLine =$ enumJSON =$ skipNothing =$ EL.map fromJSON' =$ skipNothing =$ EL.fold (++) []

friendsIds, followerIds :: Manager -> TW (Iteratee ByteString IO [Integer])
friendsIds = api "https://api.twitter.com/1/friends/ids.json" [] iterFold
followerIds = api "https://api.twitter.com/1/follower/ids.json" [] iterFold

userstream :: Iteratee Status IO a -> Manager -> TW (Iteratee ByteString IO a)
userstream iter = api "https://userstream.twitter.com/2/user.json" [] (enumLine =$ enumJSON =$ skipNothing =$ enumJsonToStatus =$ iter)
