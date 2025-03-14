module DebugPrint.Core
  ( DebugPrintRecord (..)
  , DebugPrintValue (..)
  , ToDebugPrintValue (..)
  , ToDebugPrintRecord (..)
  , ToDebugPrintValueRep
  , ToDebugPrintRecordRep
  , DebugInteger (..)
  , DebugShow (..)
  ) where

import Prelude

import Data.Foldable (toList)
import Data.Int
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import Numeric.Natural (Natural)

newtype DebugPrintRecord = DebugPrintRecord (Map Text DebugPrintValue)
  deriving newtype (Monoid, Semigroup)

data DebugPrintValue
  = DebugPrintValueInt Integer
  | DebugPrintValueText Text
  | DebugPrintValueBool Bool
  | DebugPrintValueVector (Vector DebugPrintValue)
  | DebugPrintValueRecord DebugPrintRecord

instance IsString DebugPrintValue where
  fromString = DebugPrintValueText . T.pack

---

class ToDebugPrintValue a where
  toDebugPrintValue :: a -> DebugPrintValue
  default toDebugPrintValue
    :: (Generic a, ToDebugPrintRecordRep (Rep a)) => a -> DebugPrintValue
  toDebugPrintValue = DebugPrintValueRecord . gToRecord . from

instance ToDebugPrintValue DebugPrintValue where
  toDebugPrintValue = id

instance ToDebugPrintValue Integer where
  toDebugPrintValue = DebugPrintValueInt

deriving via DebugInteger (Natural) instance ToDebugPrintValue Natural
deriving via DebugInteger (Int) instance ToDebugPrintValue Int
deriving via DebugInteger (Int8) instance ToDebugPrintValue Int8
deriving via DebugInteger (Int16) instance ToDebugPrintValue Int16
deriving via DebugInteger (Int32) instance ToDebugPrintValue Int32
deriving via DebugInteger (Int64) instance ToDebugPrintValue Int64
deriving via DebugInteger (Word) instance ToDebugPrintValue Word
deriving via DebugInteger (Word8) instance ToDebugPrintValue Word8
deriving via DebugInteger (Word16) instance ToDebugPrintValue Word16
deriving via DebugInteger (Word32) instance ToDebugPrintValue Word32
deriving via DebugInteger (Word64) instance ToDebugPrintValue Word64

instance ToDebugPrintValue Char where
  toDebugPrintValue = DebugPrintValueText . T.singleton

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

---

-- | For use with deriving-via, provides a simple 'ToDebugPrintValue'
--   instance based on 'Show' which renders as text
newtype DebugShow a = DebugShow a

instance Show a => ToDebugPrintValue (DebugShow a) where
  toDebugPrintValue (DebugShow x) =
    toDebugPrintValue $ T.pack $ show x

---

-- | For use with deriving-via, provides a simple 'ToDebugPrintValue'
--   instance based on 'toInteger' which renders as an integer
newtype DebugInteger a = DebugInteger a

instance Integral a => ToDebugPrintValue (DebugInteger a) where
  toDebugPrintValue (DebugInteger x) =
    toDebugPrintValue $ toInteger x
