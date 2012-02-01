{-# LANGUAGE DeriveDataTypeable #-}

module Web.Twitter.Utils
       ( enumJSON
       , sinkJSON
       , sinkJSON'
       , conduitParser
       , skipNothing
       , debugEE
       , fromJSON'
       )
       where

import Control.Exception
import Control.Monad.Trans
import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AT
import qualified Data.Attoparsec.Types as A
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import Data.ByteString (ByteString)
import Data.Data

data TwitterError
  = TwitterError String
  deriving (Show, Data, Typeable)

instance Exception TwitterError
 
skipNothing :: Monad m => C.Conduit (Maybe a) m a
skipNothing = undefined -- EL.concatMap (\x -> [fromJust x | isJust x])

debugEE :: (MonadIO m, Show a) => C.Conduit a m a
debugEE = undefined -- EL.mapM $ \x -> (liftIO . putStrLn . show) x >> return x

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = AT.parseMaybe parseJSON

enumJSON :: Monad m => C.Conduit ByteString m Value
enumJSON = undefined -- E.sequence $ iterParser json

sinkJSON :: C.ResourceThrow m => C.Sink ByteString m Value
sinkJSON = CA.sinkParser json

sinkJSON' :: (FromJSON a, C.ResourceIO m) => C.Sink ByteString m a
sinkJSON' = do
  v <- sinkJSON
  case fromJSON v of
    AT.Error err -> lift $ liftIO $ throwIO $ TwitterError err
    AT.Success r -> return r

conduitParser :: (CA.AttoparsecInput a, C.ResourceThrow m) => A.Parser a b -> C.Conduit a m b
conduitParser p =
  C.sequenceSink () $ \() -> do
    ret <- CA.sinkParser p
    return $ C.Emit () [ret]
