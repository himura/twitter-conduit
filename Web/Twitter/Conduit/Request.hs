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

import Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Types as HT
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Control.Lens
import Control.Applicative

-- $setup
-- >>> :set -XOverloadedStrings -XRank2Types -XEmptyDataDecls -XFlexibleInstances
-- >>> import Data.Default
-- >>> data SampleApi
-- >>> type SampleId = Integer
-- >>> instance HasCountParam (APIRequest SampleApi [SampleId])
-- >>> instance HasSinceIdParam (APIRequest SampleApi [SampleId])
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

class Parameters a where
    params :: Lens' a HT.SimpleQuery
instance Parameters (APIRequest apiName responseType) where
    params f (APIRequestGet u pa) = APIRequestGet u <$> f pa
    params f (APIRequestPost u pa) = APIRequestPost u <$> f pa
    params f (APIRequestPostMultipart u pa prt) =
        (\p -> APIRequestPostMultipart u p prt) <$> f pa

-- | This 'Prism' convert from a 'ByteString' to some value based on 'Read' and 'Show'
--
-- >>> readShow # 2
-- "2"
-- >>> "1024" ^? readShow :: Maybe Integer
-- Just 1024
readShow :: (Read a, Show a) => Prism' S.ByteString a
readShow = prism' (S8.pack . show) (readMaybe . S8.unpack)
  where
    readMaybe str = case [x | (x, t) <- reads str, ("", "") <- lex t] of
        [x] -> Just x
        _ -> Nothing

wrappedParam :: Parameters p => S.ByteString -> Prism' S.ByteString a -> Lens' p (Maybe a)
wrappedParam key aSBS = lens getter setter
   where
     getter = ((^? aSBS) =<<) . lookup key . view params
     setter = flip (over params . replace key)
     replace k (Just v) = ((k, aSBS # v):) . dropAssoc k
     replace k Nothing = dropAssoc k
     dropAssoc k = filter ((/= k) . fst)

class Parameters a => HasCountParam a where
    count :: Lens' a (Maybe Integer)
    count = wrappedParam "count" readShow
class Parameters a => HasSinceIdParam a where
    sinceId :: Lens' a (Maybe Integer)
    sinceId = wrappedParam "since_id" readShow
class Parameters a => HasMaxIdParam a where
    maxId :: Lens' a (Maybe Integer)
    maxId = wrappedParam "max_id" readShow
class Parameters a => HasCursorParam a where
    cursor :: Lens' a (Maybe Integer)
    cursor = wrappedParam "cursor" readShow
