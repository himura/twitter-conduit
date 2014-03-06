{-# LANGUAGE DeriveDataTypeable #-}

module Web.Twitter.Conduit.Error
       ( TwitterError (..)
       ) where

import Control.Exception
import Data.Data

data TwitterError
  = TwitterError String
  deriving (Show, Data, Typeable)

instance Exception TwitterError

