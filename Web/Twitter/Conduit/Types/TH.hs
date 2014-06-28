module Web.Twitter.Conduit.Types.TH
       ( makeLenses
       ) where

import Control.Lens hiding (makeLenses)
import Language.Haskell.TH

makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith (defaultRules & lensField .~ Just)
