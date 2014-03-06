{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Twitter.Conduit.Utils
       (
         sinkJSON
       , sinkFromJSON
       , showBS
       ) where

import Control.Exception
import Control.Monad.Trans.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import Data.Data
import Text.Shakespeare.Text
import Control.Monad.Logger

data TwitterError
  = TwitterError String
  deriving (Show, Data, Typeable)

instance Exception TwitterError

sinkJSON :: ( C.MonadThrow m
            , MonadLogger m
            ) => C.Consumer ByteString m Value
sinkJSON = do
    js <- CA.sinkParser json
    $(logDebug) [st|Response JSON: #{show js}|]
    return js

sinkFromJSON :: ( FromJSON a
                , C.MonadThrow m
                , MonadLogger m
                ) => C.Consumer ByteString m a
sinkFromJSON = do
    v <- sinkJSON
    case fromJSON v of
        Error err -> lift $ C.monadThrow $ TwitterError err
        Success r -> return r

showBS :: Show a => a -> ByteString
showBS = S8.pack . show
