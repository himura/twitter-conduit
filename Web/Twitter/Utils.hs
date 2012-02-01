{-# LANGUAGE DeriveDataTypeable #-}

module Web.Twitter.Utils (
  sinkJSON,
  sinkJSON',
  parseFromJSON,
  conduitParser,
  showBS,
  insertQuery,
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

conduitParser :: (CA.AttoparsecInput a, C.ResourceThrow m) => A.Parser a b -> C.Conduit a m b
conduitParser p =
  C.sequenceSink () $ \() -> do
    ret <- CA.sinkParser p
    return $ C.Emit () [ret]

sinkJSON :: C.ResourceThrow m => C.Sink ByteString m Value
sinkJSON = CA.sinkParser json

sinkJSON' :: (FromJSON a, C.ResourceIO m) => C.Sink ByteString m a
sinkJSON' = do
  v <- sinkJSON
  case fromJSON v of
    AT.Error err -> lift $ liftIO $ throwIO $ TwitterError err
    AT.Success r -> return r

-- toMaybeByteString :: Show a => a -> Maybe ByteString
-- toMaybeByteString = Just . B8.pack . show

showBS :: Show a => a -> ByteString
showBS = B8.pack . show

insertQuery :: (ByteString, Maybe ByteString) -> HT.Query -> HT.Query
insertQuery (key, value) = mk
  where mk = M.toList . M.insert key value . M.fromList
