module DebugPrint.Tagged
  ( tag
  , Tagged (..)
  ) where

import Prelude

import Data.Text qualified as T
import DebugPrint.Class
import DebugPrint.Types
import GHC.Generics (Generic)

-- | Useful for debug-printing discriminated unions
data Tagged = Tagged {tag :: T.Text, value :: DebugPrintValue}
  deriving stock (Generic)
  deriving anyclass (ToDebugPrintRecord, ToDebugPrintValue)

tag :: ToDebugPrintValue a => T.Text -> a -> DebugPrintValue
tag t = toDebugPrintValue . Tagged t . toDebugPrintValue
