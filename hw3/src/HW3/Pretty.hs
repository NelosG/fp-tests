module HW3.Pretty
  ( prettyValue
  ) where

import HW3.Base (HiValue)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = undefined
