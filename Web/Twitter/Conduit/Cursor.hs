{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Twitter.Conduit.Cursor
       ( CursorKey (..)
       , IdsCursorKey
       , UsersCursorKey
       , ListsCursorKey
       , EventsCursorKey
       , WithCursor (..)
       ) where

import Data.Aeson
import Data.Text (Text)
import Web.Twitter.Types (checkError)

-- $setup
-- >>> type UserId = Integer

class CursorKey a where
    cursorKey :: a -> Text

-- | Phantom type to specify the key which point out the content in the response.
data IdsCursorKey
instance CursorKey IdsCursorKey where
    cursorKey = const "ids"

-- | Phantom type to specify the key which point out the content in the response.
data UsersCursorKey
instance CursorKey UsersCursorKey where
    cursorKey = const "users"

-- | Phantom type to specify the key which point out the content in the response.
data ListsCursorKey
instance CursorKey ListsCursorKey where
    cursorKey = const "lists"

data EventsCursorKey
instance CursorKey EventsCursorKey where
    cursorKey = const "events"

-- | A wrapper for API responses which have "next_cursor" field.
--
-- The first type parameter of 'WithCursor' specifies the field name of contents.
--
-- >>> let Just res = decode "{\"previous_cursor\": 0, \"next_cursor\": 1234567890, \"ids\": [1111111111]}" :: Maybe (WithCursor Integer IdsCursorKey UserId)
-- >>> nextCursor res
-- Just 1234567890
-- >>> contents res
-- [1111111111]
--
-- >>> let Just res = decode "{\"previous_cursor\": 0, \"next_cursor\": 0, \"users\": [1000]}" :: Maybe (WithCursor Integer UsersCursorKey UserId)
-- >>> nextCursor res
-- Just 0
-- >>> contents res
-- [1000]
--
-- >>> let Just res = decode "{\"next_cursor\": \"hogehoge\", \"events\": [1000]}" :: Maybe (WithCursor Text EventsCursorKey UserId)
-- >>> nextCursor res
-- Just "hogehoge"
-- >>> contents res
-- [1000]
data WithCursor cursorType cursorKey wrapped = WithCursor
    { previousCursor :: Maybe cursorType
    , nextCursor :: Maybe cursorType
    , contents :: [wrapped]
    } deriving Show

instance (FromJSON wrapped, FromJSON ct, CursorKey c) =>
         FromJSON (WithCursor ct c wrapped) where
    parseJSON (Object o) = checkError o >>
      WithCursor <$> o .:? "previous_cursor"
                 <*> o .:? "next_cursor"
                 <*> o .:  cursorKey (undefined :: c)
    parseJSON _ = mempty
