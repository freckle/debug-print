{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Prelude

import Data.Aeson.QQ
import Data.Text (Text)
import DebugPrint
import DebugPrint.Aeson
import GHC.Generics (Generic)
import Test.Hspec

data Report = Report
  { milliseconds :: Int
  , errors :: [Text]
  , fileName :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToDebugPrintRecord, ToDebugPrintValue)

data Place = Place
  { country :: Text
  , region :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToDebugPrintRecord, ToDebugPrintValue)

data Journey = Journey
  { from :: Place
  , to :: Place
  }
  deriving stock (Generic)
  deriving anyclass (ToDebugPrintRecord, ToDebugPrintValue)

main :: IO ()
main = hspec $ do
  describe "debugPrintValueToAeson" $ do
    it "Omits Nothing fields" $ do
      debugPrintValueToAeson
        Report
          { milliseconds = 5_824
          , errors = ["Warning! Problems."]
          , fileName = Nothing
          }
        `shouldBe` [aesonQQ| { errors: ["Warning! Problems."], milliseconds: 5824 } |]

    it "Omits empty Vector fields" $ do
      debugPrintValueToAeson
        Report
          { milliseconds = 0
          , errors = []
          , fileName = Just "records.csv"
          }
        `shouldBe` [aesonQQ| { fileName: ["records.csv"], milliseconds: 0 } |]

    it "Omits empty Text" $ do
      debugPrintValueToAeson
        Place
          { country = "USA"
          , region = ""
          }
        `shouldBe` [aesonQQ| {country: "USA" } |]

    it "Omits empty objects" $ do
      debugPrintValueToAeson
        Journey
          { from =
              Place
                { country = "USA"
                , region = ""
                }
          , to = Place {country = "", region = ""}
          }
        `shouldBe` [aesonQQ| { from: { country: "USA" } } |]
