module Web.Twitter.Conduit.Response
       ( APIResponse (..)
       , parse
       , parsed
       ) where

import Data.Aeson
import Data.Aeson.Lens (AsJSON (..), AsPrimitive (..), AsValue (..), AsNumber (..))
import Control.Lens

data APIResponse responseType
    = APIResponse Value
    deriving Show

instance AsJSON (APIResponse a) where
    _JSON = prism' (APIResponse . toJSON) unsafeParse
instance AsPrimitive (APIResponse a)
instance AsNumber (APIResponse a)
instance AsValue (APIResponse a) where
    _Value = _JSON

unsafeParse :: FromJSON a => APIResponse ignored -> Maybe a
unsafeParse (APIResponse v)  =
    case fromJSON v of
        Success y -> Just y
        _ -> Nothing

parse :: FromJSON a => APIResponse a -> Maybe a
parse = unsafeParse

parsed :: FromJSON a => IndexPreservingGetter (APIResponse a) (Maybe a)
parsed = to parse
