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

class CursorKey a where
    cursorKey :: a -> T.Text

data IdsCursorKey
instance CursorKey IdsCursorKey where
    cursorKey = const "ids"

data UsersCursorKey
instance CursorKey UsersCursorKey where
    cursorKey = const "users"

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
