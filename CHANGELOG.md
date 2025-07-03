## [_Unreleased_](https://github.com/freckle/debug-print/compare/v0.2.1.0...main)

## [v0.2.1.0](https://github.com/freckle/debug-print/compare/v0.2.0.1...v0.2.1.0)

Add instances for `aeson` types:

- `instance ToDebugPrintValue Key`
- `instance ToDebugPrintValue Value`
- `instance ToDebugPrintValue Object`
- `instance ToDebugPrintRecord Object`

## [v0.2.0.1](https://github.com/freckle/debug-print/compare/v0.2.0.0...v0.2.0.1)

Relax lower dependency bounds

## [v0.2.0.0](https://github.com/freckle/debug-print/compare/v0.1.0.0...v0.2.0.0)

To reduce clutter in output, the default `Generic`-derived `ToDebugPrintRecord`
instance now elides any field whose values is empty text, an empty list, or an
empty object.

## [v0.1.0.0](https://github.com/freckle/debug-print/compare/v0.0.0.0...v0.1.0.0)

The parameter of `DebugPrintValueInt` is changed from `Int` to `Integer`.

Added `ToDebugPrintValue` instances for various numeric types.

Adds the `DebugInteger` newtype for deriving-via.

## [v0.0.0.0](https://github.com/freckle/debug-print/tree/v0.0.0.0)

First tagged release.
