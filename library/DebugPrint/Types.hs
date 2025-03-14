-- | A general structure to which data values may be converted
--
-- This is often useful for testing and logging to get an output that is
-- more structured and better pretty-printed than what 'show' can offer.
module DebugPrint.Types
  ( DebugPrintRecord (..)
  , DebugPrintValue (..)
  ) where

import DebugPrint.Core
