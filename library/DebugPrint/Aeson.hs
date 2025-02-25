module DebugPrint.Aeson
  ( debugPrintRecordToAeson
  , debugPrintValueToAeson
  ) where

import Prelude

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (bimap)
import Data.Map.Strict qualified as Map
import DebugPrint.Class
import DebugPrint.Types

debugPrintRecordToAeson :: ToDebugPrintRecord a => a -> Aeson.Object
debugPrintRecordToAeson a =
  let DebugPrintRecord m = toDebugPrintRecord a
  in  KeyMap.fromList
        $ bimap Key.fromText debugPrintValueToAeson
          <$> Map.toList m

debugPrintValueToAeson :: ToDebugPrintValue a => a -> Aeson.Value
debugPrintValueToAeson a = case toDebugPrintValue a of
  DebugPrintValueInt x -> Aeson.Number $ fromIntegral x
  DebugPrintValueText x -> Aeson.String x
  DebugPrintValueBool x -> Aeson.Bool x
  DebugPrintValueVector x -> Aeson.Array $ fmap debugPrintValueToAeson x
  DebugPrintValueRecord x -> Aeson.Object $ debugPrintRecordToAeson x
