-- | A general structure to which data values may be converted
--
-- This is often useful for testing and logging to get an output that is
-- more structured and better pretty-printed than what 'show' can offer.
module DebugPrint.Types
  ( DebugPrintRecord (..)
  , DebugPrintValue (..)
  ) where

import Prelude

import Data.Map (Map)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)

newtype DebugPrintRecord = DebugPrintRecord (Map Text DebugPrintValue)
  deriving newtype (Monoid, Semigroup)

data DebugPrintValue
  = DebugPrintValueInt Int
  | DebugPrintValueText Text
  | DebugPrintValueBool Bool
  | DebugPrintValueVector (Vector DebugPrintValue)
  | DebugPrintValueRecord DebugPrintRecord

instance IsString DebugPrintValue where
  fromString = DebugPrintValueText . T.pack
