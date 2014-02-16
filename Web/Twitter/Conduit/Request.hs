{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Twitter.Conduit.Request
       ( Request(..)
       , Parameters(..)
       , wrappedParam
       , sampleApiRequest
       ) where

import qualified Network.HTTP.Types as HT
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Default
import Control.Lens
import Control.Applicative

-- $setup
-- >>> :set -XOverloadedStrings -XRank2Types -XEmptyDataDecls -XFlexibleInstances

data Request a = Request
    { _method :: String
    , _url :: String
    , _params :: HT.SimpleQuery
    } deriving (Show, Eq)

class Parameters a where
    params :: Lens' a HT.SimpleQuery
instance Parameters (Request a) where
    params f (Request m u pa) = Request m u <$> f pa

isoReadShowBS :: (Show a, Read a) => Iso' (Maybe a) S.ByteString
isoReadShowBS = iso (S8.pack . showMaybe) (readMaybe . S8.unpack)
  where
    showMaybe (Just a) = show a
    showMaybe Nothing = ""
    readMaybe str = case [x | (x, t) <- reads str, ("", "") <- lex t] of
        [x] -> Just x
        _ -> Nothing

wrappedParam :: Parameters p => S.ByteString -> Iso' (Maybe a) S.ByteString -> Lens' p (Maybe a)
wrappedParam key aSBS = lens getter setter
   where
     getter = (view (from aSBS) =<<) . lookup key . view params
     setter = flip (over params . replace key)
     replace k (Just v) = ((k, view aSBS $ Just v):) . dropAssoc k
     replace k Nothing = dropAssoc k
     dropAssoc k = filter ((/= k) . fst)

class Parameters a => HasCountParam a where
    count :: Lens' a (Maybe Integer)
    count = wrappedParam "count" isoReadShowBS
class Parameters a => HasSinceIdParam a where
    sinceId :: Lens' a (Maybe Integer)
    sinceId = wrappedParam "since_id" isoReadShowBS
class Parameters a => HasMaxIdParam a where
    maxId :: Lens' a (Maybe Integer)
    maxId = wrappedParam "max_id" isoReadShowBS

-- * Example
data SampleApi
instance HasCountParam (Request SampleApi)
instance HasSinceIdParam (Request SampleApi)
instance HasMaxIdParam (Request SampleApi)

-- | make 'Request' for Sample API.
--
-- >>> sampleApiRequest ^. params
-- []
-- >>> (sampleApiRequest & count ?~ 100 & maxId ?~ 1234567890) ^. params
-- [("max_id","1234567890"),("count","100")]
-- >>> (sampleApiRequest & count ?~ 100 & maxId ?~ 1234567890 & count .~ Nothing) ^. params
-- [("max_id","1234567890")]
sampleApiRequest :: Request SampleApi
sampleApiRequest = Request "GET" "sample/api.json" def
