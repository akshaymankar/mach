module Utils.Lens where

import Lens.Micro
import Lens.Micro.TH
import Language.Haskell.TH.Syntax

myLensRules :: LensRules
myLensRules = lensRules
              & lensField .~ \_ _ n -> [TopName (mkName (nameBase n <> "L"))]
