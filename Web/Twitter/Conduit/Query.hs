module Web.Twitter.Conduit.Query (
  QueryUser(..),
  QueryList(..),
  ) where

import Web.Twitter.Conduit.Types

data QueryUser
  = QUserId UserId 
  | QScreenName String
  deriving (Show, Eq)

data QueryList
  = QListId Integer 
  | QListName String
  deriving (Show, Eq)
