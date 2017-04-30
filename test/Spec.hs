{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Protolude
import Data.Tyro

import qualified Data.Aeson as A
import Data.Text (Text)

main :: IO ()
main = defaultMain tests

tests :: [ TF.Test ]
tests = [ typeLevelJSONParserTests
        , valueLevelJSONParserTests ]


typeLevelJSONParserTests :: TF.Test
typeLevelJSONParserTests = testGroup "\nType level JSON Parser tests" . hUnitTestToTests $
  HU.TestList [ canExtractTextInJSBranch
              , canExtractIntegerInJSBranch
              , canExtractPreludeListInJSBranch
              , canExtractListInJSBranch
              , canExtractListInJSBranchWithTypeFamily ]

valueLevelJSONParserTests :: TF.Test
valueLevelJSONParserTests = testGroup "Value leve JSON Parser tests" . hUnitTestToTests $
  HU.TestList [ canUseValueLevelKeysToParseInteger
              , canUseValueLevelKeysToExtractList
              , canUseOperatorsWithoutBrackets ]

--------------------------------------------------------------------------------
-- Type level JSON Parser tests
--------------------------------------------------------------------------------

canExtractTextInJSBranch :: HU.Test
canExtractTextInJSBranch = "Can extract JSBranch on Text" ~:
  let json = "{\"key1\":{\"key2\":{\"key3\":\"Some text\"}}}"
      expected = Just "Some text"
      decoded = A.decode json :: Maybe (JSBranch ('JSKey "key1" ('JSKey "key2" ('JSKey "key3" 'JSExtract))) Text)
  in
    expected @=? fmap unwrap decoded

canExtractIntegerInJSBranch :: HU.Test
canExtractIntegerInJSBranch = "Can extract JSBranch on Integer" ~:
  let json = "{\"key1\":{\"key2\":{\"key3\":42}}}"
      expected = Just 42
      decoded = A.decode json :: Maybe (JSBranch ('JSKey "key1" ('JSKey "key2" ('JSKey "key3" 'JSExtract))) Integer)
  in
    expected @=? fmap unwrap decoded

canExtractPreludeListInJSBranch :: HU.Test
canExtractPreludeListInJSBranch = "Can extract JSBranch on List (Prelude)" ~:
  let json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}"
      expected = Just [41,42]
      decoded = A.decode json :: Maybe (JSBranch ('JSKey "key1" 'JSExtract) [JSBranch ('JSKey "key2" 'JSExtract) Integer])
  in
    expected @=? (fmap (fmap unwrap) . (fmap unwrap) $ decoded)

canExtractListInJSBranch :: HU.Test
canExtractListInJSBranch = "Can extract JSBranch on List (Tyro)" ~:
  let json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}"
      expected = Just [41,42]
      decoded = A.decode json :: Maybe (JSBranch ('JSKey "key1" ('JSArray ('JSKey "key2" 'JSExtract))) Integer)
  in
    expected @=? fmap unwrap decoded


canExtractListInJSBranchWithTypeFamily :: HU.Test
canExtractListInJSBranchWithTypeFamily = "Can extract JSBranch on List with type family" ~:
  let json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}"
      expected = Just [41,42]
      decoded = A.decode json :: Maybe ("key1" >%> List ("key2" >%> Extract Integer))
  in
    expected @=? fmap unwrap decoded


--------------------------------------------------------------------------------
-- Value level parsing tests
--------------------------------------------------------------------------------

canUseValueLevelKeysToParseInteger :: HU.Test
canUseValueLevelKeysToParseInteger =
  "Can extract integer using value level API" ~:
  let json = "{\"key1\":{\"key2\":{\"key3\":42}}}"
      parser = "key1" >%> "key2" >%> "key3" >%> extract
      expected = Just 42
      decoded = json %%> parser :: Maybe Integer
  in
    expected @=? decoded


canUseValueLevelKeysToExtractList :: HU.Test
canUseValueLevelKeysToExtractList =
  "Can extract list of integers using value level API" ~:
  let json = "{\"key1\": {\"key2\" :  [41, 42]}}"
      parser = "key1" >%> "key2" >%> extract
      expected = Just [41,42]
      decoded = json %%> parser :: Maybe [Integer]
  in
    expected @=? decoded


canUseOperatorsWithoutBrackets :: HU.Test
canUseOperatorsWithoutBrackets =
  "Can use operators without brackets in mainline case" ~:
  let json = "{\"key1\": {\"key2\" :  [41, 42]}}"
      expected = Just [41,42]
      decoded = json %%> "key1" >%> "key2" >%> extract :: Maybe [Integer]
  in
    expected @=? decoded
