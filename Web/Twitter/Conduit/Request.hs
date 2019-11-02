{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Twitter.Conduit.Request
       ( HasParam
       , APIRequest(..)
       ) where

import Data.Aeson
import GHC.TypeLits (Symbol)
import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Types as HT
import Web.Twitter.Conduit.Request.Internal

-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds -XTypeOperators
-- >>> import Control.Lens
-- >>> import Data.Default
-- >>> import Web.Twitter.Conduit.Parameters
-- >>> type SampleId = Integer
-- >>> type SampleApi = '["count" ':= Integer, "max_id" ':= Integer]
-- >>> let sampleApiRequest :: APIRequest SampleApi [SampleId]; sampleApiRequest = APIRequest "GET" "https://api.twitter.com/sample/api.json" def

-- | API request. You should use specific builder functions instead of building this directly.
--
-- For example, if there were a @SampleApi@ type and a builder function which named @sampleApiRequest@.
-- In addition, @'APIRequest' SampleApi [SampleId]@ is a instance of both of 'HasCountParam' and 'HasMaxIdParam'.
--
-- @
-- type 'SampleId' = 'Integer'
-- 'sampleApiRequest' :: 'APIRequest' 'SampleApi' ['SampleId']
-- 'sampleApiRequest' = 'APIRequest' \"GET\" \"https:\/\/api.twitter.com\/sample\/api.json\" 'def'
-- type 'SampleApi' = '[ "count" ':= Integer
--                     , "max_id" ':= Integer
--                     ]
--
-- @
--
-- We can obtain request params from @'APIRequest' SampleApi [SampleId]@ :
--
-- >>> sampleApiRequest ^. params
-- []
--
-- And update request parameters.
--
-- >>> (sampleApiRequest & #count ?~ 100 & #max_id ?~ 1234567890) ^. params
-- [("max_id",PVInteger {unPVInteger = 1234567890}),("count",PVInteger {unPVInteger = 100})]
-- >>> (sampleApiRequest & #count ?~ 100 & #max_id ?~ 1234567890 & #count .~ Nothing) ^. params
-- [("max_id",PVInteger {unPVInteger = 1234567890})]
data APIRequest (supports :: [Param Symbol *]) responseType
    = APIRequest
      { _method :: HT.Method
      , _url :: String
      , _params :: APIQuery
      }
    | APIRequestMultipart
      { _method :: HT.Method
      , _url :: String
      , _params :: APIQuery
      , _part :: [Part]
      }
    | APIRequestJSON
      { _method :: HT.Method
      , _url :: String
      , _params :: APIQuery
      , _body :: Value
      }
instance Parameters (APIRequest supports responseType) where
    type SupportParameters (APIRequest supports responseType) = supports

    params f (APIRequest m u pa) = APIRequest m u <$> f pa
    params f (APIRequestMultipart m u pa prt) =
        (\p -> APIRequestMultipart m u p prt) <$> f pa
    params f (APIRequestJSON m u pa body) = (\p -> APIRequestJSON m u p body) <$> f pa
instance Show (APIRequest apiName responseType) where
    show (APIRequest m u p) = "APIRequest " ++ show m ++ " " ++ show u ++ " " ++ show (makeSimpleQuery p)
    show (APIRequestMultipart m u p _) = "APIRequestMultipart " ++ show m ++ " " ++ show u ++ " " ++ show (makeSimpleQuery p)
    show (APIRequestJSON m u p _) = "APIRequestJSON " ++ show m ++ " " ++ show u ++ " " ++ show (makeSimpleQuery p)
