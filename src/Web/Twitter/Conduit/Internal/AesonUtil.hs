{-# LANGUAGE FlexibleContexts #-}

module Web.Twitter.Conduit.Internal.AesonUtil where

import Data.Aeson (
    Encoding,
    GFromJSON,
    GToEncoding,
    Options (fieldLabelModifier),
    Value,
    Zero,
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
 )
import Data.Aeson.Types (Parser)
import Data.List (stripPrefix)
import GHC.Generics (Generic (Rep))

stripPrefixCamelTo ::
    -- | prefix
    String ->
    String ->
    String
stripPrefixCamelTo prefix name =
    maybe name (camelTo2 '_') $ stripPrefix prefix name

aesonOptions ::
    -- | prefix
    String ->
    Options
aesonOptions prefix =
    defaultOptions {fieldLabelModifier = stripPrefixCamelTo prefix}

stripPrefixGenericParseJSON ::
    (Generic a, GFromJSON Zero (Rep a)) =>
    -- | prefix
    String ->
    Value ->
    Parser a
stripPrefixGenericParseJSON prefix = genericParseJSON $ aesonOptions prefix

stripPrefixGenericToEncoding ::
    (Generic a, GToEncoding Zero (Rep a)) =>
    -- | prefix
    String ->
    a ->
    Encoding
stripPrefixGenericToEncoding prefix = genericToEncoding $ aesonOptions prefix
