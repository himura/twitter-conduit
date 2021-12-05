{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Twitter.Conduit.Internal.APIRequest where

import Control.Lens (
    Field2 (..),
    Lens',
    Prism',
    lens,
    makeLenses,
    makePrisms,
    over,
    preview,
    to,
    traversed,
    (#),
    (%~),
    _Just,
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Client.MultipartFormData (Part)
import qualified Network.HTTP.Types as HTTPTypes

data Method
    = GET
    | POST
    | PUT
    | DELETE
    deriving (Generic, Eq, Ord, Enum, Bounded, Show, Read)

convertToHTTPMethod :: Method -> HTTPTypes.Method
convertToHTTPMethod GET = "GET"
convertToHTTPMethod POST = "POST"
convertToHTTPMethod PUT = "PUT"
convertToHTTPMethod DELETE = "DELETE"

data Param label t = label := t
type EmptyParams = ('[] :: [Param Symbol *])

type HasParam (label :: Symbol) (paramType :: *) (params :: [Param Symbol *]) = ParamType label params ~ paramType
type family ParamType (label :: Symbol) (params :: [Param Symbol *]) :: * where
    ParamType label ((label ':= paramType) ': ks) = paramType
    ParamType label ((label' ':= paramType') ': ks) = ParamType label ks

data PV
    = PVInteger {unPVInteger :: Integer}
    | PVBool {unPVBool :: Bool}
    | PVString {unPVString :: Text}
    | PVIntegerArray {unPVIntegerArray :: [Integer]}
    | PVStringArray {unPVStringArray :: [Text]}
    | PVDay {unPVDay :: Day}
    deriving (Show, Eq)
makePrisms ''PV

type APIQueryItem = (ByteString, PV)
type APIQuery = [APIQueryItem]

class ParameterValue a where
    wrapped :: Prism' PV a
instance ParameterValue Integer where
    wrapped = _PVInteger
instance ParameterValue Bool where
    wrapped = _PVBool
instance ParameterValue Text where
    wrapped = _PVString
instance ParameterValue [Integer] where
    wrapped = _PVIntegerArray
instance ParameterValue [Text] where
    wrapped = _PVStringArray
instance ParameterValue Day where
    wrapped = _PVDay

paramValueBS :: PV -> ByteString
paramValueBS (PVInteger i) = S8.pack . show $ i
paramValueBS (PVBool True) = "true"
paramValueBS (PVBool False) = "false"
paramValueBS (PVString txt) = T.encodeUtf8 txt
paramValueBS (PVIntegerArray iarr) = S8.intercalate "," $ map (S8.pack . show) iarr
paramValueBS (PVStringArray iarr) = S8.intercalate "," $ map T.encodeUtf8 iarr
paramValueBS (PVDay day) = S8.pack . show $ day

makeSimpleQuery :: APIQuery -> HTTPTypes.SimpleQuery
makeSimpleQuery = traversed . _2 %~ paramValueBS

type BodyEmpty = ()
newtype BodyMultipart = BodyMultipart [Part]
    deriving (Generic, Show)
newtype BodyJSON a = BodyJSON a
    deriving (Generic, Show, Read)

-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds -XTypeOperators
-- >>> import Control.Lens
-- >>> type SampleId = Integer
-- >>> type SampleApi = '["count" ':= Integer, "max_id" ':= Integer]
-- >>> let sampleApiRequest :: APIRequest SampleApi [SampleId]; sampleApiRequest = APIRequest "GET" "https://api.twitter.com/sample/api.json" [] BodyEmpty

-- | API request. You should use specific builder functions instead of building this directly.
--
-- For example, if there were a @SampleApi@ type and a builder function which named @sampleApiRequest@.
--
-- @
-- type SampleId = 'Integer'
-- sampleApiRequest :: 'APIRequest' SampleApi [SampleId]
-- sampleApiRequest = 'APIRequest' \"GET\" \"https:\/\/api.twitter.com\/sample\/api.json\" [] BodyEmpty
-- type SampleApi = '[ "count" ':= Integer
--                   , "max_id" ':= Integer
--                   ]
--
-- @
--
-- We can obtain request params from @'APIRequest' SampleApi [SampleId]@ :
--
-- >>> sampleApiRequest ^. params
-- []
--
-- The second type parameter of the APIRequest represents the allowed parameters for the APIRequest.
-- For example, @sampleApiRequest@ has 2 @Integer@ parameters, that is "count" and "max_id".
-- You can update those parameters by label lenses (@#count@ and @#max_id@ respectively)
--
-- >>> (sampleApiRequest & #count ?~ 100 & #max_id ?~ 1234567890) ^. params
-- [("max_id",PVInteger {unPVInteger = 1234567890}),("count",PVInteger {unPVInteger = 100})]
-- >>> (sampleApiRequest & #count ?~ 100 & #max_id ?~ 1234567890 & #count .~ Nothing) ^. params
-- [("max_id",PVInteger {unPVInteger = 1234567890})]
data APIRequest (parameters :: [Param Symbol *]) body responseType = APIRequest
    { _method :: Method
    , _url :: String
    , _params :: APIQuery
    , _body :: body
    }
    deriving (Generic, Show)
makeLenses ''APIRequest

unsafeParam ::
    ParameterValue a =>
    -- | parameter key
    ByteString ->
    Lens' (APIRequest parameters body responseType) (Maybe a)
unsafeParam key = lens (apiRequestGetParam key) (apiRequestSetParam key)

param ::
       (KnownSymbol label, ParameterValue a, HasParam label a parameters)
    => Proxy label
    -> Lens' (APIRequest parameters body responseType) (Maybe a)
param = unsafeParam . S8.pack . symbolVal

apiRequestGetParam :: ParameterValue a => ByteString -> APIRequest parameters body responseType -> Maybe a
apiRequestGetParam key = preview $ params . to (lookup key) . _Just . wrapped

apiRequestSetParam ::
       ParameterValue a
    => ByteString
    -> APIRequest parameters body responseType
    -> Maybe a
    -> APIRequest parameters body responseType
apiRequestSetParam key = flip $ over params . replace key

replace :: ParameterValue a => ByteString -> Maybe a -> APIQuery -> APIQuery
replace k (Just v) = ((k, wrapped # v) :) . dropAssoc k
replace k Nothing = dropAssoc k

dropAssoc :: ByteString -> APIQuery -> APIQuery
dropAssoc k = filter ((/= k) . fst)

instance
    ( ParameterValue a
    , KnownSymbol label
    , HasParam label a parameters
    , Functor f
    , lens ~ ((Maybe a -> f (Maybe a)) -> APIRequest parameters body responseType -> f (APIRequest parameters body responseType))
    ) =>
    IsLabel label lens
    where
    fromLabel = param (Proxy :: Proxy label)
