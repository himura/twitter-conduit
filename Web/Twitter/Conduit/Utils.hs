{-# LANGUAGE DeriveDataTypeable #-}

module Web.Twitter.Conduit.Utils (
  sinkJSON,
  sinkFromJSON,
  conduitJSON,
  conduitFromJSON,
  parseFromJSON,
  conduitParser,
  showBS,
  insertQuery,
  fromJSON',
  toMaybeByteString,
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans
import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AT
import qualified Data.Attoparsec.Types as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import Data.Data
import qualified Network.HTTP.Types as HT
import qualified Data.Map as M

data TwitterError
  = TwitterError String
  deriving (Show, Data, Typeable)

instance Exception TwitterError
 
parseFromJSON :: FromJSON a => A.Parser ByteString a
parseFromJSON = do
  v <- json
  case fromJSON v of
    AT.Error _ -> empty
    AT.Success r -> return r

conduitParser :: (CA.AttoparsecInput a, C.MonadResource m) => A.Parser a b -> C.Conduit a m b
conduitParser p =
  C.sequenceSink () $ \() -> do
    ret <- CA.sinkParser p
    return $ C.Emit () [ret]

sinkJSON :: C.MonadResource m => C.Sink ByteString m Value
sinkJSON = CA.sinkParser json

sinkFromJSON :: (FromJSON a, C.MonadResource m) => C.Sink ByteString m a
sinkFromJSON = do
  v <- sinkJSON
  case fromJSON v of
    AT.Error err -> lift $ liftIO $ throwIO $ TwitterError err
    AT.Success r -> return r

conduitJSON :: C.MonadResource m => C.Conduit ByteString m Value
conduitJSON = conduitParser json

conduitFromJSON :: (FromJSON a, C.MonadResource m) => C.Conduit ByteString m a
conduitFromJSON = conduitParser parseFromJSON

showBS :: Show a => a -> ByteString
showBS = B8.pack . show

insertQuery :: (ByteString, Maybe ByteString) -> HT.Query -> HT.Query
insertQuery (key, value) = mk
  where mk = M.toList . M.insert key value . M.fromList


fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = AT.parseMaybe parseJSON

--fromJSONSearch' :: FromJSON a => Value -> Maybe a
--fromJSONSearch' (Object o) = AT.parseMaybe (.: "results") o
--fromJSONSearch' _          = Nothing

--enumJSON :: Monad m => C.Pipe ByteString Value m a
--enumJSON = C.sequence $ CA.iterParser json

toMaybeByteString :: Show a => a -> Maybe ByteString
toMaybeByteString = Just . B8.pack . show
