{-# LANGUAGE CPP #-}

module Web.Twitter.Conduit.Types.TH
       ( makeLenses
       ) where

import Control.Lens hiding (makeLenses)
import Language.Haskell.TH

#if MIN_VERSION_lens(4, 4, 0)
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith (lensRules & lensField .~ myFieldToDef)
  where
    myFieldToDef _ n = [TopName (mkName (nameBase n))]
#else
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith (defaultRules & lensField .~ Just)
#endif
