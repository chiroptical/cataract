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
    clientSecret :: T.Text,
    requiredScopes :: T.Text,
    databaseFileName :: T.Text,
    streamerName :: T.Text,
    streamerId :: Int,
    streamerSubscriberGoal :: Int,
    streamerFollowerGoal :: Int
  }
  deriving (Show, Eq, Generic, FromJSON)

instance YamlSchema Config where
  yamlSchema =
    objectParser "Config" $
      Config
        <$> optionalFieldWithDefault "portNumber" 8081 "The servant port number"
        <*> requiredField "clientId" "Your Twitch client id"
        <*> requiredField "clientSecret" "Your Twitch client secret **keep hidden**"
        <*> optionalFieldWithDefault
          "requiredScopes"
          "user:read:email channel:read:subscriptions"
          "The required Twitch scopes"
        <*> optionalFieldWithDefault
          "databaseFileName"
          "test.db"
          "The name of the sqlite database to persist data"
        <*> requiredField
          "streamerName"
          "The name of the Twitch streamer"
        <*> optionalFieldWithDefault
          "streamerId"
          123
          "The id of the Twitch streamer"
        <*> requiredField "subscriberGoal" "How many subscribers would like to have?"
        <*> requiredField "followerGoal" "How many followers would you like to have?"
