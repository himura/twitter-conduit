{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Twitter.Conduit.Request
       ( HasParam
       , APIRequest(..)
       ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Aeson
import GHC.TypeLits (Symbol)
import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Types as HT
import Web.Twitter.Conduit.Request.Internal

-- In GHC 7.4.2, the following test fails with Overlapping instances error.
-- It may be caused by #5820 "defining instance in GHCi leads to duplicated instances".
-- So, we bypass below tests when GHC version older than 7.6.
-- see details: https://ghc.haskell.org/trac/ghc/ticket/5820
#if __GLASGOW_HASKELL__ >= 706
-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds
-- >>> import Control.Lens
-- >>> import Data.Default
-- >>> import Web.Twitter.Conduit.Parameters
-- >>> type SampleId = Integer
-- >>> type SampleApi = '["count", "max_id"]
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
-- type 'SampleApi' = '["count", "max_id"]
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
-- >>> (sampleApiRequest & count ?~ 100 & maxId ?~ 1234567890) ^. params
-- [("max_id",PVInteger {unPVInteger = 1234567890}),("count",PVInteger {unPVInteger = 100})]
-- >>> (sampleApiRequest & count ?~ 100 & maxId ?~ 1234567890 & count .~ Nothing) ^. params
-- [("max_id",PVInteger {unPVInteger = 1234567890})]
#endif
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
