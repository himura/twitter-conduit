module Web.Twitter.Enumerator.Api where

import Web.Twitter.Enumerator.Types
import Web.Twitter.Enumerator.Monad

import Network.HTTP.Enumerator
import qualified Network.HTTP.Types as HT
import Data.Enumerator (Iteratee, throwError, liftTrans)

import Data.ByteString (ByteString)

import Control.Monad.Trans

endpoint :: String
endpoint = "https://api.twitter.com/1/"

-- Method -> Url -> Query -> Iteratee
api :: ByteString -> String -> HT.Query -> Iteratee ByteString IO a -> Iteratee ByteString TW a
api m url query iter = do
  req <- lift $ apiRequest m url query
  httpMgr req (handleError iter)
  where
    handleError iter' st@(HT.Status sc _) _ =
      if 200 <= sc && sc < 300
      then iter'
      else throwError $ HTTPStatusCodeException st

httpMgr :: Request IO
        -> (HT.Status
            -> HT.ResponseHeaders
            -> Iteratee ByteString IO a)
        -> Iteratee ByteString TW a
httpMgr req iterf = do        
  mgr <- lift getManager
  liftTrans $ http req iterf mgr


apiRequest :: ByteString -> String -> HT.Query -> TW (Request IO)
apiRequest m uri query = do
  p <- getProxy
  req <- liftIO $ parseUrl uri >>= \r ->
    return $ r { method = m, queryString = query, proxy = p }
  signOAuthTW req