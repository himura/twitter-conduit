{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Twitter.Conduit.Cursor
       ( CursorKey (..)
       , IdsCursorKey
       , UsersCursorKey
       , WithCursor (..)
       ) where

import Web.Twitter.Types (checkError)
import qualified Data.Text as T
import Data.Aeson
import Data.Monoid
import Control.Applicative

-- $setup
-- >>> type UserId = Integer

class CursorKey a where
    cursorKey :: a -> T.Text

data IdsCursorKey
instance CursorKey IdsCursorKey where
    cursorKey = const "ids"

data UsersCursorKey
instance CursorKey UsersCursorKey where
    cursorKey = const "users"

-- | A wrapper for API responses which have "next_cursor" field.
--
-- The first type parameter of 'WithCursor' specifies the field name of contents.
--
-- >>> let Just res = decode "{\"previous_cursor\": 0, \"next_cursor\": 1234567890, \"ids\": [1111111111]}" :: Maybe (WithCursor IdsCursorKey UserId)
-- >>> nextCursor res
-- 1234567890
-- >>> contents res
-- [1111111111]
--
-- >>> let Just res = decode "{\"previous_cursor\": 0, \"next_cursor\": 0, \"users\": [1000]}" :: Maybe (WithCursor UsersCursorKey UserId)
-- >>> nextCursor res
-- 0
-- >>> contents res
-- [1000]
data WithCursor cursorKey wrapped = WithCursor
    { previousCursor :: Integer
    , nextCursor :: Integer
    , contents :: [wrapped]
    }

instance (FromJSON wrapped, CursorKey c) =>
         FromJSON (WithCursor c wrapped) where
    parseJSON (Object o) = checkError o >>
      WithCursor <$> o .:  "previous_cursor"
                 <*> o .:  "next_cursor"
                 <*> o .:  cursorKey (undefined :: c)
    parseJSON _ = mempty
