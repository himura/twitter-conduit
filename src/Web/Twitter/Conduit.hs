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
module Web.Twitter.Conduit (
    -- * How to use this library
    -- $howto

    -- ** Authentication
    -- $auth

    -- ** How to call API
    -- $call

    -- *** How to specify API parameters
    -- $parameter

    -- *** Conduit API: Recursive API call with changing cursor parameter
    -- $conduit

    -- * Re-exports
    module Web.Twitter.Conduit.Api,
    module Web.Twitter.Conduit.Cursor,
    module Web.Twitter.Conduit.Request,
    module Web.Twitter.Conduit.Response,
    module Web.Twitter.Conduit.Stream,
    module Web.Twitter.Conduit.Types,

    -- * 'Web.Twitter.Conduit.Base'
    call,
    call',
    callWithResponse,
    callWithResponse',
    sourceWithMaxId,
    sourceWithMaxId',
    sourceWithCursor,
    sourceWithCursor',
    sourceWithSearchResult,
    sourceWithSearchResult',

    -- * 'Web.Twitter.Conduit.Parameters'
    ListParam (..),
    MediaData (..),
    UserListParam (..),
    UserParam (..),
    TweetMode (..),

    -- * re-exports
    OAuth (..),
    Credential (..),
    def,
    Manager,
    newManager,
    tlsManagerSettings,
) where

import Web.Twitter.Conduit.Api
import Web.Twitter.Conduit.Base
import Web.Twitter.Conduit.Cursor
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Request
import Web.Twitter.Conduit.Response
import Web.Twitter.Conduit.Stream
import Web.Twitter.Conduit.Types

import Data.Default (def)
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import Web.Authenticate.OAuth

-- for haddock

import Control.Lens
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T

{-# ANN module "HLint: ignore Use import/export shortcut" #-}

-- $howto
--
-- The main module of twitter-conduit is "Web.Twitter.Conduit".
-- This library cooperate with <http://hackage.haskell.org/package/twitter-types twitter-types>,
-- <http://hackage.haskell.org/package/authenticate-oauth authenticate-oauth>,
-- and <http://hackage.haskell.org/package/conduit conduit>.
-- All of following examples import modules as below:
--
-- @
-- {\-# LANGUAGE OverloadedLabels #-\}
-- {\-# LANGUAGE OverloadedStrings #-\}
--
-- import Web.Twitter.Conduit
-- import Web.Twitter.Types.Lens
-- import Data.Conduit
-- import qualified Data.Conduit.List as CL
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import Control.Monad.IO.Class
-- import Control.Lens
-- @

-- $auth
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
-- @
-- twInfo = 'setCredential' tokens credential 'def'
-- @

-- $call
--
-- Twitter API requests are performed by 'call' function.
-- For example, <https://dev.twitter.com/docs/api/1.1/get/statuses/home_timeline GET statuses/home_timeline>
-- could be obtained by:
--
-- @
-- mgr \<- 'newManager' 'tlsManagerSettings'
-- timeline \<- 'call' twInfo mgr 'statusesHomeTimeline'
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
-- timeline \<- 'call' twInfo mgr '$' 'statusesHomeTimeline' '&' #count '?~' 200
-- @

-- $parameter
--
-- The parameters which can be specified for this API, is able to be obtained from type parameters of APIRequest.
-- For example,
--
-- @
-- 'statusesHomeTimeline' ::
--     'APIRequest' 'StatusesHomeTimeline' ['Web.Twitter.Types.Status']
-- @
--
-- - The 2nd type parameter of 'APIRequest' represents acceptable parameters in this API request.
-- - The 3nd type parameter of 'APIRequest' denotes a return type of this API request.
--
-- The 2nd type parameter of 'statusesHomeTimeline' is 'StatusesHomeTimeline' defined as below:
--
-- @
-- type StatusesHomeTimeline =
--     '[ "count" ':= Integer
--      , "since_id" ':= Integer
--      , "max_id" ':= Integer
--      , "trim_user" ':= Bool
--      , "exclude_replies" ':= Bool
--      , "contributor_details" ':= Bool
--      , "include_entities" ':= Bool
--      , "tweet_mode" ':= TweetMode
--      ]
-- @
--
-- Each element of list represents the name and type of API parameters.
-- You can specify those parameter with OverloadedLabels extension.
-- In the above example, it shows that 'statusesHomeTimeline' API supports "tweet_mode" parameter with 'TweetMode' type, so
-- you can specify "tweet_mode" parameter as:
--
-- @
-- 'statusesHomeTimeline' & #tweet_mode ?~ 'Extended'
-- @

-- $conduit
--
-- If you need more statuses, you can obtain those with multiple API requests.
-- This library provides the wrapper for multiple requests with conduit interfaces.
-- For example, if you intend to fetch the all friends information,
-- you may perform multiple API requests with changing cursor parameter,
-- or use the conduit wrapper 'sourceWithCursor' as below:
--
-- @
-- friends \<-
--     'runConduit' $
--         'sourceWithMaxId' twInfo mgr ('friendsList' ('ScreenNameParam' \"thimura\") '&' #count '?~' 200)
--             '.|' 'CL.consume'
-- @
--
-- Statuses APIs, for instance, 'homeTimeline', are also wrapped by 'sourceWithMaxId'.
--
-- For example, you can print 60 tweets from your home timeline, as below:
--
-- @
-- main :: IO ()
-- main = do
--     mgr \<- 'newManager' 'tlsManagerSettings'
--     'runConduit' $ 'sourceWithMaxId' twInfo mgr 'homeTimeline'
--         '.|' CL.isolate 60
--         '.|' CL.mapM_
--             (\\status -> do
--                  T.putStrLn $
--                      T.concat
--                          [ T.pack . show $ status ^. statusId
--                          , \": \"
--                          , status ^. statusUser . userScreenName
--                          , \": \"
--                          , status ^. statusText
--                          ]
--             )
-- @
