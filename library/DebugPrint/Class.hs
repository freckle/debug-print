-- | Generic-derivable classes for converting to 'DebugPrintRecord' and 'Value'
--
-- These classes are designed to be used only for pretty-printing in tests
-- and logging. They should generally be used anywhere you would derive 'Show'.
module DebugPrint.Class
  ( ToDebugPrintValue (..)
  , ToDebugPrintRecord (..)
  , ToDebugPrintValueRep
  , ToDebugPrintRecordRep
  ) where

import DebugPrint.Core
