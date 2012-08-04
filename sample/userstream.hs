import Control.Monad.Trans
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Web.Twitter.Conduit
import Common

main :: IO ()
main = withCF $ do
  src <- userstream
  src C.$$+- CL.mapM_ (lift . lift . print)
