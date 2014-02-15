{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Twitter.Conduit.Utils
       (
         sinkJSON
       , sinkFromJSON
       , conduitJSON
       , conduitFromJSON
       , showBS
       , insertQuery
       , fromJSON'
       ) where

import Control.Exception
import Control.Monad.Trans.Class
import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AT
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Attoparsec as CA
import Data.Data
import qualified Network.HTTP.Types as HT
import qualified Data.Map as M
import Text.Shakespeare.Text
import Control.Monad.Logger

-- $setup
-- >>> :set -XOverloadedStrings

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
        AT.Error err -> lift $ C.monadThrow $ TwitterError err
        AT.Success r -> return r

conduitJSON :: ( C.MonadThrow m
               , MonadLogger m
               ) => C.Conduit ByteString m Value
conduitJSON = CL.sequence sinkJSON

conduitFromJSON :: ( FromJSON a
                   , C.MonadThrow m
                   , MonadLogger m
                   ) => C.Conduit ByteString m a
conduitFromJSON = CL.sequence sinkFromJSON

showBS :: Show a => a -> ByteString
showBS = B8.pack . show

-- | insert key-value pair to @SimpleQuery@.
--
-- >>> insertQuery ("Key1", "Value1") []
-- [("Key1","Value1")]
-- >>> insertQuery ("Key1", "Value1") [("Test","Input")]
-- [("Key1","Value1"),("Test","Input")]
insertQuery :: (ByteString, ByteString) -> HT.SimpleQuery -> HT.SimpleQuery
insertQuery (key, value) = mk
  where mk = M.toList . M.insert key value . M.fromList

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = AT.parseMaybe parseJSON


