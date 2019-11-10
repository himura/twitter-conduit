{-# LANGUAGE OverloadedStrings #-}

module TestUtils (getTWInfo) where

import qualified Data.ByteString.Char8 as S8
import System.Environment
import Web.Twitter.Conduit

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where
    getEnv' = (S8.pack <$>) . getEnv

getTWInfo :: IO TWInfo
getTWInfo = do
    (oa, cred) <- getOAuthTokens
    return $ setCredential oa cred def
