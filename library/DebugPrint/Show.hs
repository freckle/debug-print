module DebugPrint.Show
  ( DebugShow (..)
  ) where

import Prelude

import Data.Text qualified as T
import DebugPrint.Class

-- | For use with deriving-via, provides a simple 'ToDebugPrintValue'
--   instance based on 'Show' which renders as text
newtype DebugShow a = DebugShow a

instance Show a => ToDebugPrintValue (DebugShow a) where
  toDebugPrintValue (DebugShow x) =
    toDebugPrintValue $ T.pack $ show x
