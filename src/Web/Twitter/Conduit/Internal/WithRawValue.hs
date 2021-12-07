{-# LANGUAGE DeriveGeneric #-}

module Web.Twitter.Conduit.Internal.WithRawValue where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value)
import GHC.Generics (Generic)

data WithRawValue a = WithRawValue
    { withRawValueRaw :: Value
    , withRawValueParsed :: a
    }
    deriving (Generic, Eq, Ord, Show, Read)
instance FromJSON a => FromJSON (WithRawValue a) where
    parseJSON v = WithRawValue v <$> parseJSON v
instance ToJSON (WithRawValue a) where
    toJSON = toJSON . withRawValueRaw

instance NFData a => NFData (WithRawValue a)
