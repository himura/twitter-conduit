{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Web.Twitter.Conduit.Cursor
       ( CursorKey (..)
       , IdsCursorKey
       , UsersCursorKey
       , ListsCursorKey
       , WithCursor (..)
       ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Monoid
#endif
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

#if __GLASGOW_HASKELL__ >= 706
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
#endif
data WithCursor cursorKey wrapped = WithCursor
    { previousCursor :: Integer
    , nextCursor :: Integer
    , contents :: [wrapped]
    } deriving Show

instance (FromJSON wrapped, CursorKey c) =>
         FromJSON (WithCursor c wrapped) where
    parseJSON (Object o) = checkError o >>
      WithCursor <$> o .:  "previous_cursor"
                 <*> o .:  "next_cursor"
                 <*> o .:  cursorKey (undefined :: c)
    parseJSON _ = mempty
