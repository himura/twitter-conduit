module Web.Twitter.Status (
  statuses,
  
  -- * Timelines
  homeTimeline,
  mentions,
  publicTimeline,
  retweetedByMe,
  retweetedToMe,
  retweetsOfMe,
  userTimeline,
  retweetedToUser,
  retweetedByUser,
  idRetweetedBy,
  idRetweetedByIds,
  retweetsId
  ) where

import Data.Aeson
import qualified Data.Conduit as C
import qualified Network.HTTP.Types as HT

import Web.Twitter.Api
import Web.Twitter.Monad
import Web.Twitter.Types

statuses :: (FromJSON a, Show a) => String -> HT.Query -> C.Source TW a
statuses url query = apiWithPages ("statuses/" ++ url) query

homeTimeline :: HT.Query -> C.Source TW Status
homeTimeline = statuses "home_timeline.json"

mentions :: HT.Query -> C.Source TW Status
mentions = statuses "mentions.json"

publicTimeline :: HT.Query -> C.Source TW Status
publicTimeline = statuses "public_timeline.json"

retweetedByMe :: HT.Query -> C.Source TW Status
retweetedByMe = statuses "retweeted_by_me.json"

retweetedToMe :: HT.Query -> C.Source TW Status
retweetedToMe = statuses "retweeted_to_me.json"

retweetsOfMe :: HT.Query -> C.Source TW Status
retweetsOfMe = statuses "retweeted_of_me.json"

userTimeline :: HT.Query -> C.Source TW Status
userTimeline = statuses "user_timeline.json"

retweetedToUser :: HT.Query -> C.Source TW Status
retweetedToUser = statuses "retweeted_to_user.json"

retweetedByUser :: HT.Query -> C.Source TW Status
retweetedByUser = statuses "retweeted_by_user.json"

idRetweetedBy :: StatusId -> HT.Query -> C.Source TW User
idRetweetedBy status_id = statuses (show status_id ++ "/retweeted_by.json")

idRetweetedByIds :: StatusId -> HT.Query -> C.Source TW UserId
idRetweetedByIds status_id = statuses (show status_id ++ "/retweeted_by/ids.json")

retweetsId :: StatusId -> HT.Query -> TW [RetweetedStatus]
retweetsId status_id query = apiGet uri query
  where uri = "statuses/retweets/" ++ show status_id ++ ".json"

showId :: StatusId -> HT.Query -> TW Status
showId status_id query =
  apiGet ("statuses/show/" ++ show status_id ++ ".json") query
