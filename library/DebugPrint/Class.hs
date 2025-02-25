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

import Prelude

import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as V
import DebugPrint.Types
import GHC.Generics
import Numeric.Natural (Natural)

class ToDebugPrintValue a where
  toDebugPrintValue :: a -> DebugPrintValue
  default toDebugPrintValue
    :: (Generic a, ToDebugPrintRecordRep (Rep a)) => a -> DebugPrintValue
  toDebugPrintValue = DebugPrintValueRecord . gToRecord . from

instance ToDebugPrintValue DebugPrintValue where
  toDebugPrintValue = id

instance ToDebugPrintValue Int where
  toDebugPrintValue = DebugPrintValueInt

instance ToDebugPrintValue Char where
  toDebugPrintValue = DebugPrintValueText . T.singleton

instance ToDebugPrintValue Natural where
  toDebugPrintValue = DebugPrintValueInt . fromIntegral

instance ToDebugPrintValue T.Text where
  toDebugPrintValue = DebugPrintValueText

instance ToDebugPrintValue TL.Text where
  toDebugPrintValue = DebugPrintValueText . TL.toStrict

instance ToDebugPrintValue Bool where
  toDebugPrintValue = DebugPrintValueBool

instance ToDebugPrintValue a => ToDebugPrintValue (Maybe a) where
  toDebugPrintValue = toDebugPrintValue . toList

instance ToDebugPrintValue a => ToDebugPrintValue [a] where
  toDebugPrintValue = toDebugPrintValue . V.fromList

instance ToDebugPrintValue a => ToDebugPrintValue (NonEmpty a) where
  toDebugPrintValue = toDebugPrintValue . toList

instance ToDebugPrintValue a => ToDebugPrintValue (Seq a) where
  toDebugPrintValue = toDebugPrintValue . toList

instance ToDebugPrintValue a => ToDebugPrintValue (Vector a) where
  toDebugPrintValue = DebugPrintValueVector . fmap toDebugPrintValue

---

class ToDebugPrintRecord a where
  toDebugPrintRecord :: a -> DebugPrintRecord
  default toDebugPrintRecord
    :: (Generic a, ToDebugPrintRecordRep (Rep a)) => a -> DebugPrintRecord
  toDebugPrintRecord = gToRecord . from

instance ToDebugPrintRecord DebugPrintRecord where
  toDebugPrintRecord = id

---

class ToDebugPrintValueRep (f :: Type -> Type) where
  gToValue :: f a -> DebugPrintValue

instance ToDebugPrintValue a => ToDebugPrintValueRep (K1 i a) where
  gToValue (K1 x) = toDebugPrintValue x

---

class ToDebugPrintRecordRep (f :: Type -> Type) where
  gToRecord :: f a -> DebugPrintRecord

instance ToDebugPrintRecordRep U1 where
  gToRecord _ = DebugPrintRecord mempty

instance
  (ToDebugPrintRecordRep f, ToDebugPrintRecordRep g)
  => ToDebugPrintRecordRep (f :*: g)
  where
  gToRecord (x :*: y) = gToRecord x <> gToRecord y

instance ToDebugPrintRecordRep f => ToDebugPrintRecordRep (D1 i f) where
  gToRecord (M1 x) = gToRecord x

instance ToDebugPrintRecordRep f => ToDebugPrintRecordRep (C1 i f) where
  gToRecord (M1 x) = gToRecord x

instance (Selector s, ToDebugPrintValueRep f) => ToDebugPrintRecordRep (S1 s f) where
  gToRecord s1@(M1 x) = DebugPrintRecord $ Map.singleton (T.pack (selName s1)) (gToValue x)
