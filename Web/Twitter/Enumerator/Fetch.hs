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
       )
       where

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Status
import Web.Twitter.Enumerator.Monad
import Web.Twitter.Enumerator.Utils

import Web.Authenticate.OAuth

import Data.Aeson hiding (Error)
import Data.Attoparsec (parse, maybeResult)

import Network.HTTP.Enumerator
import Data.Enumerator hiding (map, filter)
import qualified Data.Enumerator.List as EL

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B

import qualified Data.Vector as V

import Control.Monad.State
import Data.Maybe

api :: String -> Iteratee ByteString IO a -> Manager -> TW (Iteratee ByteString IO a)
api url iter mgr = do
  env <- get
  req <- liftIO $ signOAuth (twOAuth env) (twCredential env) =<< parseUrl url
  return $ http req (\s h -> iter) mgr

statuses url = api aurl iter
  where iter = enumLine =$ enumJSON =$ skipNothing =$ enumJsonToStatus =$ EL.consume
        aurl = "https://api.twitter.com/1/statuses/" ++ url

statusesPublicTimeline = statuses "public_timeline.json"
statusesUserTimeline :: Manager -> TW (Iteratee ByteString IO [Status])
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
friendsIds = api "https://api.twitter.com/1/friends/ids.json" iterFold
followerIds = api "https://api.twitter.com/1/follower/ids.json" iterFold

