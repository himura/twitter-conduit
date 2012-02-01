module Web.Twitter.Query (
  QueryUser(..),
  QueryList(..),
  ) where

import Web.Twitter.Types

data QueryUser
  = QUserId UserId 
  | QScreenName String
  deriving (Show, Eq)

data QueryList
  = QListId Integer 
  | QListName String
  deriving (Show, Eq)
