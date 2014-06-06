{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Twitter.Conduit.Request
       ( APIRequest(..)
       ) where

import Web.Twitter.Conduit.Parameters

import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Types as HT
import Control.Applicative

-- $setup
-- >>> :set -XOverloadedStrings -XRank2Types -XEmptyDataDecls -XFlexibleInstances -XOverlappingInstances -XIncoherentInstances
-- >>> import Control.Lens
-- >>> import Data.Default
-- >>> data SampleApi
-- >>> type SampleId = Integer
-- >>> instance HasCountParam (APIRequest SampleApi [SampleId])
-- >>> instance HasMaxIdParam (APIRequest SampleApi [SampleId])
-- >>> let sampleApiRequest :: APIRequest SampleApi [SampleId]; sampleApiRequest = APIRequestGet "https://api.twitter.com/sample/api.json" def

-- | API request. You should use specific builder functions instead of building this directly.
--
-- For example, if there were a @SampleApi@ type and a builder function which named @sampleApiRequest@.
-- In addition, @'APIRequest' SampleApi [SampleId]@ is a instance of both of 'HasCountParam' and 'HasMaxIdParam'.
--
-- @
-- data 'SampleApi'
-- type 'SampleId' = 'Integer'
-- instance 'HasCountParam' ('APIRequest' 'SampleApi' ['SampleId'])
-- instance 'HasMaxIdParam' ('APIRequest' 'SampleApi' ['SampleId'])
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
-- >>> (sampleApiRequest & count ?~ 100 & maxId ?~ 1234567890) ^. params
-- [("max_id","1234567890"),("count","100")]
-- >>> (sampleApiRequest & count ?~ 100 & maxId ?~ 1234567890 & count .~ Nothing) ^. params
-- [("max_id","1234567890")]
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
instance Show (APIRequest apiName responseType) where
    show (APIRequestGet u p) = "APIRequestGet " ++ show u ++ " " ++ show p
    show (APIRequestPost u p) = "APIRequestPost " ++ show u ++ " " ++ show p
    show (APIRequestPostMultipart u p _) = "APIRequestPostMultipart " ++ show u ++ " " ++ show p
