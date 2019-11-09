{-# LANGUAGE OverloadedStrings #-}

-- Example:
--   $ export OAUTH_CONSUMER_KEY="your consumer key"
--   $ export OAUTH_CONSUMER_SECRET="your consumer secret"
--   $ runhaskell oauth_callback.hs

module Main where

import Web.Scotty
import qualified Network.HTTP.Types as HT
import Web.Twitter.Conduit
import qualified Web.Authenticate.OAuth as OA
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as M
import Data.Maybe
import Data.IORef
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe

callback :: String
callback = "http://localhost:3000/callback"

getTokens :: IO OAuth
getTokens = do
    consumerKey <- getEnv "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv "OAUTH_CONSUMER_SECRET"
    return $
        twitterOAuth
        { oauthConsumerKey = S8.pack consumerKey
        , oauthConsumerSecret = S8.pack consumerSecret
        , oauthCallback = Just $ S8.pack callback
        }

type OAuthToken = S.ByteString

usersToken :: IORef (M.Map OAuthToken Credential)
usersToken = unsafePerformIO $ newIORef M.empty

takeCredential :: OAuthToken -> IORef (M.Map OAuthToken Credential) -> IO (Maybe Credential)
takeCredential k ioref =
    atomicModifyIORef ioref $ \m ->
        let (res, newm) = M.updateLookupWithKey (\_ _ -> Nothing) k m in
        (newm, res)

storeCredential :: OAuthToken -> Credential -> IORef (M.Map OAuthToken Credential) -> IO ()
storeCredential k cred ioref =
    atomicModifyIORef ioref $ \m -> (M.insert k cred m, ())

main :: IO ()
main = do
    tokens <- getTokens
    mgr <- newManager tlsManagerSettings
    putStrLn $ "browse URL: http://localhost:3000/signIn"
    scotty 3000 $ app tokens mgr

makeMessage :: OAuth -> Credential -> S.ByteString
makeMessage tokens (Credential cred) =
    S8.intercalate "\n"
        [ "export OAUTH_CONSUMER_KEY=\"" <> oauthConsumerKey tokens <> "\""
        , "export OAUTH_CONSUMER_SECRET=\"" <> oauthConsumerSecret tokens <> "\""
        , "export OAUTH_ACCESS_TOKEN=\"" <> fromMaybe "" (lookup "oauth_token" cred) <> "\""
        , "export OAUTH_ACCESS_SECRET=\"" <> fromMaybe "" (lookup "oauth_token_secret" cred) <> "\""
        ]

app :: OAuth -> Manager -> ScottyM ()
app tokens mgr = do
    get "/callback" $ do
        temporaryToken <- param "oauth_token"
        oauthVerifier <- param "oauth_verifier"
        mcred <- liftIO $ takeCredential temporaryToken usersToken
        case mcred of
            Just cred -> do
                accessTokens <- OA.getAccessToken tokens (OA.insert "oauth_verifier" oauthVerifier cred) mgr
                liftIO $ print accessTokens

                let message = makeMessage tokens accessTokens
                liftIO . S8.putStrLn $ message
                text . LT.pack . S8.unpack $ message

            Nothing -> do
                status HT.status404
                text "temporary token is not found"

    get "/signIn" $ do
        cred <- OA.getTemporaryCredential tokens mgr
        case lookup "oauth_token" $ unCredential cred of
            Just temporaryToken -> do
                liftIO $ storeCredential temporaryToken cred usersToken
                let url = OA.authorizeUrl tokens cred
                redirect $ LT.pack url
            Nothing -> do
                status HT.status500
                text "Failed to obtain the temporary token."
