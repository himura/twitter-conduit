{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Twitter.Conduit.Cursor (
    IdsCursorKey,
    UsersCursorKey,
    ListsCursorKey,
    EventsCursorKey,
    WithCursor (..),
) where

-- import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Proxy (Proxy (..))
import Data.String
import GHC.Generics
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- $setup
-- >>> import Data.Text
-- >>> type UserId = Integer

type IdsCursorKey = "ids"
type UsersCursorKey = "users"
type ListsCursorKey = "lists"
type EventsCursorKey = "events"

-- | A wrapper for API responses which have "next_cursor" field.
--
-- The first type parameter of 'WithCursor' specifies the field name of contents.
--
-- >>> let Just res = decode "{\"previous_cursor\": 0, \"next_cursor\": 1234567890, \"ids\": [1111111111]}" :: Maybe (WithCursor Integer "ids" UserId)
-- >>> nextCursor res
-- Just 1234567890
-- >>> contents res
-- [1111111111]
--
-- >>> let Just res = decode "{\"previous_cursor\": 0, \"next_cursor\": 0, \"users\": [1000]}" :: Maybe (WithCursor Integer "users" UserId)
-- >>> nextCursor res
-- Just 0
-- >>> contents res
-- [1000]
--
-- >>> let Just res = decode "{\"next_cursor\": \"hogehoge\", \"events\": [1000]}" :: Maybe (WithCursor Text "events" UserId)
-- >>> nextCursor res
-- Just "hogehoge"
-- >>> contents res
-- [1000]
data WithCursor cursorType (cursorKey :: Symbol) wrapped = WithCursor
    { previousCursor :: Maybe cursorType
    , nextCursor :: Maybe cursorType
    , contents :: [wrapped]
    }
    deriving (Show, Eq, Generic, Generic1, Functor, Foldable, Traversable)

instance (KnownSymbol cursorKey, FromJSON cursorType) => FromJSON1 (WithCursor cursorType cursorKey) where
    liftParseJSON _ lp =
        withObject ("WithCursor \"" ++ cursorKeyStr ++ "\"") $ \obj ->
            WithCursor <$> obj .:? "previous_cursor"
                <*> obj .:? "next_cursor"
                <*> (obj .: fromString cursorKeyStr >>= lp)
      where
        cursorKeyStr = symbolVal (Proxy :: Proxy cursorKey)

instance (KnownSymbol cursorKey, FromJSON cursorType, FromJSON wrapped) => FromJSON (WithCursor cursorType cursorKey wrapped) where
    parseJSON = parseJSON1

-- instance NFData a => NFData (WithCursor cursorType cursorKey wrapped)
