{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import YamlParse.Applicative
  ( YamlSchema (yamlSchema),
    objectParser,
    optionalFieldWithDefault,
    requiredField,
  )

data Config = Config
  { portNumber :: Int,
    clientId :: T.Text,
    requiredScopes :: T.Text
  }
  deriving (Show, Eq, Generic, FromJSON)

instance YamlSchema Config where
  yamlSchema =
    objectParser "Config" $
      Config
        <$> optionalFieldWithDefault "portNumber" 8081 "The servant port number"
        <*> requiredField "clientId" "Your Twitch client id"
        <*> optionalFieldWithDefault
          "requiredScopes"
          "user:read:email channel:read:subscriptions"
          "The required Twitch scopes"
