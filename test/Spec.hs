{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Protolude
import Data.Jason

import qualified Data.Aeson as A
import Data.Text (Text)

main :: IO ()
main = defaultMain tests

tests :: [ TF.Test ]
tests = [ genericJSONParserTests ]


genericJSONParserTests :: TF.Test
genericJSONParserTests = testGroup "\nGeneric JSON Parser tests" . hUnitTestToTests $
  HU.TestList [ canExtractTextInJSBranch
              , canExtractIntegerInJSBranch
              , canExtractListInJSBranch
              , canExtractListInJSBranchWithTypeFamily ]


--------------------------------------------------------------------------------
-- Generic JSON Parser tests
--------------------------------------------------------------------------------

canExtractTextInJSBranch :: HU.Test
canExtractTextInJSBranch = "Can extract JSBranch on Text" ~:
  let json = "{\"key1\":{\"key2\":{\"key3\":\"Some text\"}}}"
      expected = Just "Some text"
      decoded = A.decode json :: Maybe (JSBranch '["key1", "key2", "key3"] Text)
  in
    expected @=? fmap unwrap decoded

canExtractIntegerInJSBranch :: HU.Test
canExtractIntegerInJSBranch = "Can extract JSBranch on Integer" ~:
  let json = "{\"key1\":{\"key2\":{\"key3\":42}}}"
      expected = Just 42
      decoded = A.decode json :: Maybe (JSBranch '["key1", "key2", "key3"] Integer)
  in
    expected @=? fmap unwrap decoded

canExtractListInJSBranch :: HU.Test
canExtractListInJSBranch = "Can extract JSBranch on List" ~:
  let json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}"
      expected = Just [41,42]
      decoded = A.decode json :: Maybe (JSBranch '["key1"] [JSBranch '["key2"] Integer])
  in
    expected @=? (fmap (fmap unwrap) . (fmap unwrap) $ decoded)


canExtractListInJSBranchWithTypeFamily :: HU.Test
canExtractListInJSBranchWithTypeFamily = "Can extract JSBranch on List with type family" ~:
  let json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}"
      expected = Just [41,42]
      decoded = A.decode json :: Maybe ("key1" |>| List ("key2" |>| Parse Integer))
  in
    expected @=? (fmap (fmap unwrap) . (fmap unwrap) $ decoded)
