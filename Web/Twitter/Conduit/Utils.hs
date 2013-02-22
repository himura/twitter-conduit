{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

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

data TwitterError
  = TwitterError String
  deriving (Show, Data, Typeable)

instance Exception TwitterError

#if MIN_VERSION_conduit(1,0,0)
sinkJSON :: C.MonadResource m => C.Consumer ByteString m Value
#else
sinkJSON :: C.MonadResource m => C.GLSink ByteString m Value
#endif
sinkJSON = CA.sinkParser json

#if MIN_VERSION_conduit(1,0,0)
sinkFromJSON :: (FromJSON a, C.MonadResource m) => C.Consumer ByteString m a
#else
sinkFromJSON :: (FromJSON a, C.MonadResource m) => C.GLSink ByteString m a
#endif
sinkFromJSON = do
  v <- sinkJSON
  case fromJSON v of
    AT.Error err -> lift $ C.monadThrow $ TwitterError err
    AT.Success r -> return r

#if MIN_VERSION_conduit(1,0,0)
conduitJSON :: C.MonadResource m => C.Conduit ByteString m Value
#else
conduitJSON :: C.MonadResource m => C.GLInfConduit ByteString m Value
#endif
conduitJSON = CL.sequence $ sinkJSON

conduitFromJSON :: (FromJSON a, C.MonadResource m) => C.Conduit ByteString m a
conduitFromJSON = CL.sequence $ sinkFromJSON

showBS :: Show a => a -> ByteString
showBS = B8.pack . show

insertQuery :: (ByteString, ByteString) -> HT.SimpleQuery -> HT.SimpleQuery
insertQuery (key, value) = mk
  where mk = M.toList . M.insert key value . M.fromList

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = AT.parseMaybe parseJSON


