{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
#ifdef TWITTER_LOGGING
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
#endif

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
#ifdef TWITTER_LOGGING
import Text.Shakespeare.Text
import Control.Monad.Logger
#endif

data TwitterError
  = TwitterError String
  deriving (Show, Data, Typeable)

instance Exception TwitterError

sinkJSON :: ( C.MonadThrow m
#ifdef TWITTER_LOGGING
            , MonadLogger m
#endif
            ) => C.Consumer ByteString m Value
sinkJSON = do
  js <- CA.sinkParser json
#ifdef TWITTER_LOGGING
  $(logDebug) [st|Response JSON: #{show js}|]
#endif
  return js

sinkFromJSON :: ( FromJSON a
                , C.MonadThrow m
#ifdef TWITTER_LOGGING
                , MonadLogger m
#endif
                ) => C.Consumer ByteString m a
sinkFromJSON = do
  v <- sinkJSON
  case fromJSON v of
    AT.Error err -> lift $ C.monadThrow $ TwitterError err
    AT.Success r -> return r

conduitJSON :: ( C.MonadThrow m
#ifdef TWITTER_LOGGING
               , MonadLogger m
#endif
               ) => C.Conduit ByteString m Value
conduitJSON = CL.sequence $ sinkJSON

conduitFromJSON :: ( FromJSON a
                   , C.MonadThrow m
#ifdef TWITTER_LOGGING
                   , MonadLogger m
#endif
                   ) => C.Conduit ByteString m a
conduitFromJSON = CL.sequence $ sinkFromJSON

showBS :: Show a => a -> ByteString
showBS = B8.pack . show

insertQuery :: (ByteString, ByteString) -> HT.SimpleQuery -> HT.SimpleQuery
insertQuery (key, value) = mk
  where mk = M.toList . M.insert key value . M.fromList

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = AT.parseMaybe parseJSON


