{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "Profile page" $ do
    it "asserts no access to my-account for anonymous users" $ do
      get ProfileR
      statusIs 403

    it "asserts access to my-account for authenticated users" $ do
      authenticateAs "12345"
      get ProfileR
      statusIs 200

    it "asserts user's information is shown" $ do
      let userIdent = "12345"
      authenticateAs userIdent
      get ProfileR
      htmlAnyContain ".username" . unpack $ userIdent
