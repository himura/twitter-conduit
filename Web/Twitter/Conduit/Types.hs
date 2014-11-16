module Web.Twitter.Conduit.Types
       ( TWToken (..)
       , TWInfo (..)
       , twitterOAuth
       , setCredential
       ) where

import Data.Default
import Web.Authenticate.OAuth
import Network.HTTP.Conduit

data TWToken = TWToken
    { twOAuth :: OAuth
    , twCredential :: Credential
    } deriving (Show, Eq)
instance Default TWToken where
    def = TWToken twitterOAuth (Credential [])

data TWInfo = TWInfo
    { twToken :: TWToken
    , twProxy :: Maybe Proxy
    } deriving (Show, Eq)
instance Default TWInfo where
    def = TWInfo
        { twToken = def
        , twProxy = Nothing
        }

twitterOAuth :: OAuth
twitterOAuth =
    def { oauthServerName = "twitter"
        , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
        , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
        , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
        , oauthConsumerKey = error "You MUST specify oauthConsumerKey parameter."
        , oauthConsumerSecret = error "You MUST specify oauthConsumerSecret parameter."
        , oauthSignatureMethod = HMACSHA1
        , oauthCallback = Nothing
        }

-- | set OAuth keys and Credentials to TWInfo.
--
-- >>> let proxy = Proxy "localhost" 8080
-- >>> let twinfo = def { twProxy = Just proxy }
-- >>> let oauth = twitterOAuth { oauthConsumerKey = "consumer_key", oauthConsumerSecret = "consumer_secret" }
-- >>> let credential = Credential [("oauth_token","...")]
-- >>> let twinfo2 = setCredential oauth credential twinfo
-- >>> oauthConsumerKey . twOAuth . twToken $ twinfo2
-- "consumer_key"
-- >>> twProxy twinfo2 == Just proxy
-- True
setCredential :: OAuth -> Credential -> TWInfo -> TWInfo
setCredential oa cred env
  = TWInfo
    { twToken = TWToken oa cred
    , twProxy = twProxy env
    }
