{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}

module Web.Twitter.Conduit.Request
       ( Parameters(..)
       , APIRequest(..)
       , APIQuery
       , APIQueryItem
       , PV(..)
       , makeSimpleQuery
       , paramValueBS
       ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Calendar (Day)
import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Types as HT

class Parameters a where
    params :: Lens' a APIQuery

-- In GHC 7.4.2, the following test fails with Overlapping instances error.
-- It may be caused by #5820 "defining instance in GHCi leads to duplicated instances".
-- So, we bypass below tests when GHC version older than 7.6.
-- see details: https://ghc.haskell.org/trac/ghc/ticket/5820
#if __GLASGOW_HASKELL__ >= 706
-- $setup
-- >>> :set -XOverloadedStrings -XRank2Types -XEmptyDataDecls -XFlexibleInstances
-- >>> import Control.Lens
-- >>> import Data.Default
-- >>> import Web.Twitter.Conduit.Parameters
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
-- [("max_id",PVInteger {unPVInteger = 1234567890}),("count",PVInteger {unPVInteger = 100})]
-- >>> (sampleApiRequest & count ?~ 100 & maxId ?~ 1234567890 & count .~ Nothing) ^. params
-- [("max_id",PVInteger {unPVInteger = 1234567890})]
#endif
data APIRequest apiName responseType
    = APIRequestGet
      { _url :: String
      , _params :: APIQuery
      }
    | APIRequestPost
      { _url :: String
      , _params :: APIQuery
      }
    | APIRequestPostMultipart
      { _url :: String
      , _params :: APIQuery
      , _part :: [Part]
      }
instance Parameters (APIRequest apiName responseType) where
    params f (APIRequestGet u pa) = APIRequestGet u <$> f pa
    params f (APIRequestPost u pa) = APIRequestPost u <$> f pa
    params f (APIRequestPostMultipart u pa prt) =
        (\p -> APIRequestPostMultipart u p prt) <$> f pa
instance Show (APIRequest apiName responseType) where
    show (APIRequestGet u p) = "APIRequestGet " ++ show u ++ " " ++ show (makeSimpleQuery p)
    show (APIRequestPost u p) = "APIRequestPost " ++ show u ++ " " ++ show (makeSimpleQuery p)
    show (APIRequestPostMultipart u p _) = "APIRequestPostMultipart " ++ show u ++ " " ++ show (makeSimpleQuery p)

type APIQuery = [APIQueryItem]
type APIQueryItem = (ByteString, PV)

data PV
    = PVInteger { unPVInteger :: Integer }
    | PVBool { unPVBool :: Bool }
    | PVString { unPVString :: Text }
    | PVIntegerArray { unPVIntegerArray :: [Integer] }
    | PVStringArray { unPVStringArray :: [Text] }
    | PVDay { unPVDay :: Day }
    deriving (Show, Eq)

makeSimpleQuery :: APIQuery -> HT.SimpleQuery
makeSimpleQuery = traversed . _2 %~ paramValueBS

paramValueBS :: PV -> ByteString
paramValueBS (PVInteger i) = S8.pack . show $ i
paramValueBS (PVBool True) = "true"
paramValueBS (PVBool False) = "false"
paramValueBS (PVString txt) = T.encodeUtf8 txt
paramValueBS (PVIntegerArray iarr) = S8.intercalate "," $ map (S8.pack . show) iarr
paramValueBS (PVStringArray iarr) = S8.intercalate "," $ map T.encodeUtf8 iarr
paramValueBS (PVDay day) = S8.pack . show $ day
