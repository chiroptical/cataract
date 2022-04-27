module Data.EncryptSpec (spec) where

import TestImport

import Data.Encrypt
import Hedgehog (evalEither)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Settings (EncryptionSettings (..))
import Test.Hspec.Hedgehog (
  forAll,
  hedgehog,
  (===),
 )

spec :: Spec
spec = runIO $
  hspec $ do
    describe "roundtrip encryption" $ do
      it "can roundtrip all examples" $
        hedgehog $ do
          originalText <- forAll $ Gen.text (Range.linear 1 100) Gen.unicode
          let encryptionSettings =
                EncryptionSettings
                  { encryptionSettingsCipherSecretKey = "LG43ovFqO0wVMIOh+PlXOtVJ4WNqO1rW0yLcXEiV9ow="
                  }
          -- Roundtrip the 'originalText' through encryption and decryption
          eRoundtripText <-
            lift $
              runEncryptM encryptionSettings $
                decryptText =<< encryptText originalText
          -- The result should be a 'Right' and match the 'originalText'
          roundtripText <- evalEither eRoundtripText
          originalText === roundtripText
