{-# LANGUAGE FlexibleContexts #-}

module Web.Twitter.Conduit.Internal.AesonUtil where

import Data.Aeson
    ( Encoding
    , GFromJSON
    , GToEncoding
    , Options(fieldLabelModifier)
    , Value
    , Zero
    , camelTo2
    , defaultOptions
    , genericParseJSON
    , genericToEncoding
    )
import Data.List (stripPrefix)
import Data.Aeson.Types ( Parser )
import GHC.Generics (Generic(Rep))

stripPrefixCamelTo ::
       String -- ^ prefix
    -> String
    -> String
stripPrefixCamelTo prefix name =
    maybe name (camelTo2 '_') $ stripPrefix prefix name

aesonOptions ::
       String -- ^ prefix
    -> Options
aesonOptions prefix =
    defaultOptions {fieldLabelModifier = stripPrefixCamelTo prefix}


stripPrefixGenericParseJSON ::
       (Generic a, GFromJSON Zero (Rep a))
    => String -- ^ prefix
    -> Value
    -> Parser a
stripPrefixGenericParseJSON prefix = genericParseJSON $ aesonOptions prefix

stripPrefixGenericToEncoding ::
       (Generic a, GToEncoding Zero (Rep a))
    => String -- ^ prefix
    -> a
    -> Encoding
stripPrefixGenericToEncoding prefix = genericToEncoding $ aesonOptions prefix
