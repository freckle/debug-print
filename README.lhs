# debug-print

Offers `ToDebugPrintValue`, a structured alternative to `Show`
for printing values for testing and debugging purposes.

<!--
```haskell
module Main (main) where

import Prelude

import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Markdown.Unlit ()
import Test.Hspec
```
-->

```haskell
import DebugPrint
import DebugPrint.Aeson
import Data.Aeson qualified as Aeson
```

```haskell
data Report = Report
  { milliseconds :: Int
  , errors :: [Text]
  }
  deriving stock Generic
  deriving anyclass (ToDebugPrintRecord, ToDebugPrintValue)

report :: Report
report = Report{ milliseconds = 5_824
               , errors = ["Warning! Problems."] }
```

```haskell
main :: IO ()
main = hspec $ do
  it "" $ do
    Aeson.encode (debugPrintValueToAeson report) `shouldBe`
      "{\"errors\":[\"Warning! Problems.\"],\"milliseconds\":5824}"
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
