{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module:      Web.Twitter.Conduit
-- Copyright:   (c) 2014 Takahiro Himura
-- License:     BSD
-- Maintainer:  Takahiro Himura <taka@himura.jp>
-- Stability:   experimental
-- Portability: portable
--
-- A client library for Twitter APIs (see <https://dev.twitter.com/>).

module Web.Twitter.Conduit
       (
       -- * How to use this library
       -- $howto

       -- * Re-exports
         module Web.Twitter.Conduit.Api
       , module Web.Twitter.Conduit.Cursor
       , module Web.Twitter.Conduit.Request
       , module Web.Twitter.Conduit.Response
       , module Web.Twitter.Conduit.Status
       , module Web.Twitter.Conduit.Stream
       , module Web.Twitter.Conduit.Types

       -- * 'Web.Twitter.Conduit.Base'
       , call
       , call'
       , callWithResponse
       , callWithResponse'
       , sourceWithMaxId
       , sourceWithMaxId'
       , sourceWithCursor
       , sourceWithCursor'
       , sourceWithSearchResult
       , sourceWithSearchResult'

       -- * 'Web.Twitter.Conduit.Parameters'
       , Parameters.ListParam(..)
       , Parameters.MediaData(..)
       , Parameters.UserListParam(..)
       , Parameters.UserParam(..)

       -- * re-exports
       , OAuth (..)
       , Credential (..)
       , def
       , Manager
       , newManager
       , tlsManagerSettings

       -- * deprecated
       , contributorDetails
       , count
       , cursor
       , displayCoordinates
       , excludeReplies
       , filterLevel
       , follow
       , inReplyToStatusId
       , includeEntities
       , includeMyRetweet
       , includeRts
       , includeUserEntities
       , lang
       , language
       , locale
       , map
       , maxId
       , mediaIds
       , page
       , possiblySensitive
       , replies
       , sinceId
       , skipStatus
       , stallWarnings
       , trimUser
       , until
       ) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Cursor
import qualified Web.Twitter.Conduit.Parameters as Parameters
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Response
import Web.Twitter.Conduit.Status
import Web.Twitter.Conduit.Stream
import Web.Twitter.Conduit.Types
import Web.Twitter.Types

import Data.Default (def)
import Data.Time.Calendar (Day)
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import Web.Authenticate.OAuth

import Prelude hiding (map, until)

-- for haddock
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Lens

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif

-- $howto
--
-- The main module of twitter-conduit is "Web.Twitter.Conduit".
-- This library cooperate with <http://hackage.haskell.org/package/twitter-types twitter-types>,
-- <http://hackage.haskell.org/package/authenticate-oauth authenticate-oauth>,
-- and <http://hackage.haskell.org/package/conduit conduit>.
-- All of following examples import modules as below:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Web.Twitter.Conduit
-- > import Web.Twitter.Types.Lens
-- > import Data.Conduit
-- > import qualified Data.Conduit.List as CL
-- > import qualified Data.Text as T
-- > import qualified Data.Text.IO as T
-- > import Control.Monad.IO.Class
-- > import Control.Lens
--
-- First, you should obtain consumer token and secret from <https://apps.twitter.com/ Twitter>,
-- and prepare 'OAuth' variables as follows:
--
-- @
-- tokens :: 'OAuth'
-- tokens = 'twitterOAuth'
--     { 'oauthConsumerKey' = \"YOUR CONSUMER KEY\"
--     , 'oauthConsumerSecret' = \"YOUR CONSUMER SECRET\"
--     }
-- @
--
-- Second, you should obtain access token and secret.
-- You can find examples obtaining those tokens in
-- <https://github.com/himura/twitter-conduit/blob/master/sample sample directry>,
-- for instance,
-- <https://github.com/himura/twitter-conduit/blob/master/sample/oauth_pin.hs oauth_pin.hs>, and
-- <https://github.com/himura/twitter-conduit/blob/master/sample/oauth_callback.hs oauth_callback.hs>.
-- If you need more information, see <https://dev.twitter.com/docs/auth/obtaining-access-tokens>.
--
-- You should set an access token to 'Credential' variable:
--
-- @
-- credential :: 'Credential'
-- credential = 'Credential'
--     [ (\"oauth_token\", \"YOUR ACCESS TOKEN\")
--     , (\"oauth_token_secret\", \"YOUR ACCESS TOKEN SECRET\")
--     ]
-- @
--
-- You should also set up the 'TWToken' and 'TWInfo' variables as below:
--
-- @
-- twInfo :: 'TWInfo'
-- twInfo = 'def'
--     { 'twToken' = 'def' { 'twOAuth' = tokens, 'twCredential' = credential }
--     , 'twProxy' = Nothing
--     }
-- @
--
-- Or, simply as follows:
--
-- > twInfo = setCredential tokens credential def
--
-- Twitter API requests are performed by 'call' function.
-- For example, <https://dev.twitter.com/docs/api/1.1/get/statuses/home_timeline GET statuses/home_timeline>
-- could be obtained by:
--
-- @
-- mgr \<- 'newManager' 'tlsManagerSettings'
-- timeline \<- 'call' twInfo mgr 'homeTimeline'
-- @
--
-- The response of 'call' function is wrapped by the suitable type of
-- <http://hackage.haskell.org/package/twitter-types twitter-types>.
-- In this case, /timeline/ has a type of ['Status'].
-- If you need /raw/ JSON Value which is parsed by <http://hackage.haskell.org/package/aeson aeson>,
-- use 'call'' to obtain it.
--
-- By default, the response of <https://dev.twitter.com/docs/api/1.1/get/statuses/home_timeline GET statuses/home_timeline>
-- includes 20 tweets, and you can change the number of tweets by the /count/ parameter.
--
-- @
-- timeline \<- 'call' twInfo mgr '$' 'homeTimeline' '&' 'count' '?~' 200
-- @
--
-- If you need more statuses, you can obtain those with multiple API requests.
-- This library provides the wrapper for multiple requests with conduit interfaces.
-- For example, if you intend to fetch the all friends information,
-- you may perform multiple API requests with changing cursor parameter,
-- or use the conduit wrapper 'sourceWithCursor' as below:
--
-- @
-- friends \<- 'sourceWithCursor' twInfo mgr ('friendsList' ('ScreenNameParam' \"thimura\") '&' 'count' '?~' 200) '$$' 'CL.consume'
-- @
--
-- Statuses APIs, for instance, 'homeTimeline', are also wrapped by 'sourceWithMaxId'.
--
-- For example, you can print 1000 tweets from your home timeline, as below:
--
-- @
-- main :: IO ()
-- main = do
--     mgr \<- 'newManager' 'tlsManagerSettings'
--     'sourceWithMaxId' twInfo mgr 'homeTimeline'
--         $= CL.isolate 60
--         $$ CL.mapM_ $ \\status -> liftIO $ do
--             T.putStrLn $ T.concat [ T.pack . show $ status ^. statusId
--                                   , \": \"
--                                   , status ^. statusUser . userScreenName
--                                   , \": \"
--                                   , status ^. statusText
--                                   ]
-- @

contributorDetails :: Parameters.HasContributorDetailsParam a => Lens' a (Maybe Bool)
contributorDetails = Parameters.contributorDetails
{-# DEPRECATED contributorDetails "Please use Web.Twitter.Conduit.Parameters.contributorDetails" #-}

count :: Parameters.HasCountParam a => Lens' a (Maybe Integer)
count = Parameters.count
{-# DEPRECATED count "Please use Web.Twitter.Conduit.Parameters.count" #-}

cursor :: Parameters.HasCursorParam a => Lens' a (Maybe Integer)
cursor = Parameters.cursor
{-# DEPRECATED cursor "Please use Web.Twitter.Conduit.Parameters.cursor" #-}

displayCoordinates :: Parameters.HasDisplayCoordinatesParam a => Lens' a (Maybe Bool)
displayCoordinates = Parameters.displayCoordinates
{-# DEPRECATED displayCoordinates "Please use Web.Twitter.Conduit.Parameters.displayCoordinates" #-}

excludeReplies :: Parameters.HasExcludeRepliesParam a => Lens' a (Maybe Bool)
excludeReplies = Parameters.excludeReplies
{-# DEPRECATED excludeReplies "Please use Web.Twitter.Conduit.Parameters.excludeReplies" #-}

filterLevel :: Parameters.HasFilterLevelParam a => Lens' a (Maybe T.Text)
filterLevel = Parameters.filterLevel
{-# DEPRECATED filterLevel "Please use Web.Twitter.Conduit.Parameters.filterLevel" #-}

follow :: Parameters.HasFollowParam a => Lens' a (Maybe Bool)
follow = Parameters.follow
{-# DEPRECATED follow "Please use Web.Twitter.Conduit.Parameters.follow" #-}

inReplyToStatusId :: Parameters.HasInReplyToStatusIdParam a => Lens' a (Maybe Integer)
inReplyToStatusId = Parameters.inReplyToStatusId
{-# DEPRECATED inReplyToStatusId "Please use Web.Twitter.Conduit.Parameters.inReplyToStatusId" #-}

includeEntities :: Parameters.HasIncludeEntitiesParam a => Lens' a (Maybe Bool)
includeEntities = Parameters.includeEntities
{-# DEPRECATED includeEntities "Please use Web.Twitter.Conduit.Parameters.includeEntities" #-}

includeMyRetweet :: Parameters.HasIncludeMyRetweetParam a => Lens' a (Maybe Bool)
includeMyRetweet = Parameters.includeMyRetweet
{-# DEPRECATED includeMyRetweet "Please use Web.Twitter.Conduit.Parameters.includeMyRetweet" #-}

includeRts :: Parameters.HasIncludeRtsParam a => Lens' a (Maybe Bool)
includeRts = Parameters.includeRts
{-# DEPRECATED includeRts "Please use Web.Twitter.Conduit.Parameters.includeRts" #-}

includeUserEntities :: Parameters.HasIncludeUserEntitiesParam a => Lens' a (Maybe Bool)
includeUserEntities = Parameters.includeUserEntities
{-# DEPRECATED includeUserEntities "Please use Web.Twitter.Conduit.Parameters.includeUserEntities" #-}

lang :: Parameters.HasLangParam a => Lens' a (Maybe T.Text)
lang = Parameters.lang
{-# DEPRECATED lang "Please use Web.Twitter.Conduit.Parameters.lang" #-}

language :: Parameters.HasLanguageParam a => Lens' a (Maybe T.Text)
language = Parameters.language
{-# DEPRECATED language "Please use Web.Twitter.Conduit.Parameters.language" #-}

locale :: Parameters.HasLocaleParam a => Lens' a (Maybe T.Text)
locale = Parameters.locale
{-# DEPRECATED locale "Please use Web.Twitter.Conduit.Parameters.locale" #-}

map :: Parameters.HasMapParam a => Lens' a (Maybe Bool)
map = Parameters.map
{-# DEPRECATED map "Please use Web.Twitter.Conduit.Parameters.map" #-}

maxId :: Parameters.HasMaxIdParam a => Lens' a (Maybe Integer)
maxId = Parameters.maxId
{-# DEPRECATED maxId "Please use Web.Twitter.Conduit.Parameters.maxId" #-}

mediaIds :: Parameters.HasMediaIdsParam a => Lens' a (Maybe [Integer])
mediaIds = Parameters.mediaIds
{-# DEPRECATED mediaIds "Please use Web.Twitter.Conduit.Parameters.mediaIds" #-}

page :: Parameters.HasPageParam a => Lens' a (Maybe Integer)
page = Parameters.page
{-# DEPRECATED page "Please use Web.Twitter.Conduit.Parameters.page" #-}

possiblySensitive :: Parameters.HasPossiblySensitiveParam a => Lens' a (Maybe Bool)
possiblySensitive = Parameters.possiblySensitive
{-# DEPRECATED possiblySensitive "Please use Web.Twitter.Conduit.Parameters.possiblySensitive" #-}

replies :: Parameters.HasRepliesParam a => Lens' a (Maybe T.Text)
replies = Parameters.replies
{-# DEPRECATED replies "Please use Web.Twitter.Conduit.Parameters.replies" #-}

sinceId :: Parameters.HasSinceIdParam a => Lens' a (Maybe Integer)
sinceId = Parameters.sinceId
{-# DEPRECATED sinceId "Please use Web.Twitter.Conduit.Parameters.sinceId" #-}

skipStatus :: Parameters.HasSkipStatusParam a => Lens' a (Maybe Bool)
skipStatus = Parameters.skipStatus
{-# DEPRECATED skipStatus "Please use Web.Twitter.Conduit.Parameters.skipStatus" #-}

stallWarnings :: Parameters.HasStallWarningsParam a => Lens' a (Maybe Bool)
stallWarnings = Parameters.stallWarnings
{-# DEPRECATED stallWarnings "Please use Web.Twitter.Conduit.Parameters.stallWarnings" #-}

trimUser :: Parameters.HasTrimUserParam a => Lens' a (Maybe Bool)
trimUser = Parameters.trimUser
{-# DEPRECATED trimUser "Please use Web.Twitter.Conduit.Parameters.trimUser" #-}

until :: Parameters.HasUntilParam a => Lens' a (Maybe Day)
until = Parameters.until
{-# DEPRECATED until "Please use Web.Twitter.Conduit.Parameters.until" #-}
