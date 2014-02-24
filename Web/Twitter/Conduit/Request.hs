{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Twitter.Conduit.Request
       ( APIRequest(..)
       , Parameters(..)
       , wrappedParam
       , HasSinceIdParam(..)
       , HasCountParam(..)
       , HasMaxIdParam(..)
       , HasCursorParam(..)
       ) where

import Web.Twitter.Conduit.Parameters

import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Types as HT
import Control.Applicative

-- $setup
-- >>> :set -XOverloadedStrings -XRank2Types -XEmptyDataDecls -XFlexibleInstances
-- >>> import Control.Lens
-- >>> import Data.Default
-- >>> data SampleApi
-- >>> type SampleId = Integer
-- >>> class Parameters a => HasTestIdParam a where { testId :: Lens' a (Maybe Integer); testId = wrappedParam "test_id" readShow }
-- >>> class Parameters a => HasTestUserIdParam a where { testUserId :: Lens' a (Maybe Integer); testUserId = wrappedParam "test_user_id" readShow }
-- >>> instance HasTestIdParam (APIRequest SampleApi [SampleId])
-- >>> instance HasTestUserIdParam (APIRequest SampleApi [SampleId])
-- >>> let sampleApiRequest :: APIRequest SampleApi [SampleId]; sampleApiRequest = APIRequestGet "https://api.twitter.com/sample/api.json" def

-- | API request. You should use specific builder functions instead of building this directly.
--
-- For example, if there were a @SampleApi@ type and a builder function which named @sampleApiRequest@.
-- In addition, @'APIRequest' SampleApi [SampleId]@ is a instance of both of 'HasCountParam' and 'HasMaxIdParam'.
--
-- @
-- data 'SampleApi'
-- type 'SampleId' = 'Integer'
-- instance 'HasTestIdParam' ('APIRequest' 'SampleApi' ['SampleId'])
-- instance 'HasUserNameParam' ('APIRequest' 'SampleApi' ['SampleId'])
-- 'sampleApiRequest' :: 'APIRequest' 'SampleApi' ['SampleId']
-- 'sampleApiRequest' = 'APIRequestGet' \"https:\/\/api.twitter.com\/sample\/api.json\" 'def'
-- @
--
-- We can obtain request params from @'APIRequest' SampleApi [SampleId]@ :
--
-- >>> sampleApiRequest ^. params
-- []
--
-- And update request parameters.
--
-- >>> (sampleApiRequest & testId ?~ 100 & testUserId ?~ 1234567) ^. params
-- [("test_user_id","1234567"),("test_id","100")]
-- >>> (sampleApiRequest & testId ?~ 100 & testUserId ?~ 1234567 & testId .~ Nothing) ^. params
-- [("test_user_id","1234567")]
data APIRequest apiName responseType
    = APIRequestGet
      { _url :: String
      , _params :: HT.SimpleQuery
      }
    | APIRequestPost
      { _url :: String
      , _params :: HT.SimpleQuery
      }
    | APIRequestPostMultipart
      { _url :: String
      , _params :: HT.SimpleQuery
      , _part :: [Part]
      }
instance Parameters (APIRequest apiName responseType) where
    params f (APIRequestGet u pa) = APIRequestGet u <$> f pa
    params f (APIRequestPost u pa) = APIRequestPost u <$> f pa
    params f (APIRequestPostMultipart u pa prt) =
        (\p -> APIRequestPostMultipart u p prt) <$> f pa
