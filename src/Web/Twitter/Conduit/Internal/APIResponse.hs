{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Twitter.Conduit.Internal.APIResponse where

import Control.Exception (Exception, throwIO)
import Control.Lens (makeLensesFor)
import Control.Monad ((<=<))
import Data.Aeson (
    FromJSON (parseJSON),
    Result (Error, Success),
    ToJSON (toEncoding),
    Value,
    eitherDecode',
    fromJSON,
    json,
 )
import Data.Attoparsec.ByteString (eitherResult, endOfInput, parseOnly, parseWith)
import Data.Attoparsec.ByteString.Char8 (decimal, signed)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (ResponseHeaders, Status)
import qualified Network.HTTP.Types as HTTPTypes
import Web.Twitter.Conduit.Internal.AesonUtil (
    stripPrefixGenericParseJSON,
    stripPrefixGenericToEncoding,
 )

data RateLimitStatus = RateLimitStatus
    { -- | the rate limit ceiling for that given endpoint (HTTP header: x-rate-limit-limit)
      rateLimitStatusLimit :: Maybe Int
    , -- | the number of requests left for the 15-minute window (HTTP header: x-rate-limit-remaining)
      rateLimitStatusRemaining :: Maybe Int
    , -- | the remaining window before the rate limit resets, in UTC epoch seconds (HTTP header: x-rate-limit-reset)
      rateLimitStatusReset :: Maybe Int
    }
    deriving (Generic, Eq, Ord, Show, Read)

data APIResponse responseType = APIResponse
    { apiResponseStatus :: Status
    , apiResponseHeaders :: ResponseHeaders
    , apiResponseRateLimitStatus :: RateLimitStatus
    , apiResponseBody :: responseType
    }
    deriving (Generic, Eq, Functor, Foldable, Traversable, Show)
makeLensesFor
    [ ("apiResponseStatus", "responseStatusL")
    , ("apiResponseHeaders", "responseHeadersL")
    , ("apiResponseRateLimitStatus", "rateLimitStatusL")
    , ("apiResponseBody", "responseBodyL")
    ]
    ''APIResponse

data APIException = APIException
    { apiExceptionStatus :: Status
    , apiExceptionHeaders :: ResponseHeaders
    , apiExceptionContext :: APIExceptionContent
    }
    deriving (Generic, Eq, Ord, Show, Typeable)
instance Exception APIException

data APIExceptionContent
    = RateLimitExceeded RateLimitStatus TwitterErrors
    | ErrorResponse TwitterErrors
    | UnknownErrorResponse L.ByteString
    | ParseJSONError String
    | FromJSONError String Value
    deriving (Generic, Eq, Ord, Show, Read, Typeable)

newtype TwitterErrors = TwitterErrors
    { twitterErrors :: [TwitterError]
    }
    deriving (Generic, Eq, Ord, Show, Read, Typeable)

instance FromJSON TwitterErrors where
    parseJSON = stripPrefixGenericParseJSON "twitter"

instance ToJSON TwitterErrors where
    toEncoding = stripPrefixGenericToEncoding "twitter"

-- | Twitter Error Messages
--
-- see detail: <https://developer.twitter.com/en/docs/twitter-ads-api/response-codes>
data TwitterError = TwitterError
    { twitterErrorCode :: Int
    , twitterErrorMessage :: T.Text
    }
    deriving (Generic, Eq, Ord, Show, Read, Typeable)

instance Enum TwitterError where
    fromEnum = twitterErrorCode
    toEnum a = TwitterError a T.empty

instance FromJSON TwitterError where
    parseJSON = stripPrefixGenericParseJSON "twitterError"
instance ToJSON TwitterError where
    toEncoding = stripPrefixGenericToEncoding "twitterError"

getRateLimitStatus :: HTTP.Response a -> RateLimitStatus
getRateLimitStatus res =
    RateLimitStatus
        { rateLimitStatusLimit = getHeaderInt "x-rate-limit-limit"
        , rateLimitStatusRemaining = getHeaderInt "x-rate-limit-remaining"
        , rateLimitStatusReset = getHeaderInt "x-rate-limit-reset"
        }
  where
    getHeaderInt name = lookup name (HTTP.responseHeaders res) >>= parseInt
    parseInt = either (const Nothing) Just . parseOnly (signed decimal <* endOfInput)

eitherDecodeBodyReader :: HTTP.BodyReader -> IO (Either APIExceptionContent Value)
eitherDecodeBodyReader br =
    eitherFormatError <$> parseWith br json S.empty
  where
    eitherFormatError = either (Left . ParseJSONError) Right . eitherResult

eitherFromJSON :: FromJSON a => Value -> Either APIExceptionContent a
eitherFromJSON value =
    case fromJSON value of
        Success body -> Right body
        Error err -> Left $ FromJSONError err value

class ResponseBodyType a where
    handleResponse :: HTTP.Response HTTP.BodyReader -> IO (Either APIException (APIResponse a))

type NoContent = ()
instance ResponseBodyType () where
    handleResponse = handleNoContentResponse

instance {-# OVERLAPPABLE #-} FromJSON a => ResponseBodyType a where
    handleResponse = handleJSONResponse

handleNoContentResponse ::
    HTTP.Response HTTP.BodyReader ->
    IO (Either APIException (APIResponse ()))
handleNoContentResponse res =
    case HTTP.responseStatus res of
        st
            | st == HTTPTypes.status204 ->
                return . Right $
                    APIResponse
                        { apiResponseStatus = st
                        , apiResponseHeaders = HTTP.responseHeaders res
                        , apiResponseRateLimitStatus = getRateLimitStatus res
                        , apiResponseBody = ()
                        }
            | otherwise -> Left <$> handleErrorResponse res

handleErrorResponse :: HTTP.Response HTTP.BodyReader -> IO APIException
handleErrorResponse res = do
    lbs <- HTTP.brReadSome (HTTP.responseBody res) 4096
    let err =
            either (const $ UnknownErrorResponse lbs) ErrorResponse $
                eitherDecode' lbs
    return $
        APIException (HTTP.responseStatus res) (HTTP.responseHeaders res) err

handleJSONResponse ::
    FromJSON responseType =>
    HTTP.Response HTTP.BodyReader ->
    IO (Either APIException (APIResponse responseType))
handleJSONResponse res =
    case HTTP.responseStatus res of
        st
            | st >= HTTPTypes.status200 && st < HTTPTypes.status300 -> do
                deserResult <- eitherDecodeBodyReader $ HTTP.responseBody res
                case deserResult >>= eitherFromJSON of
                    Left err ->
                        return . Left $
                            APIException st (HTTP.responseHeaders res) err
                    Right value ->
                        return . Right $
                            APIResponse
                                { apiResponseStatus = st
                                , apiResponseHeaders = HTTP.responseHeaders res
                                , apiResponseRateLimitStatus =
                                    getRateLimitStatus res
                                , apiResponseBody = value
                                }
            | otherwise -> Left <$> handleErrorResponse res

handleResponseThrow ::
    ResponseBodyType responseType =>
    HTTP.Response HTTP.BodyReader ->
    IO (APIResponse responseType)
handleResponseThrow = either throwIO return <=< handleResponse
