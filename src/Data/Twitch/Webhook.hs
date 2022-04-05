{-# LANGUAGE TemplateHaskell #-}

module Data.Twitch.Webhook where

import Import.NoFoundation
import Data.Aeson.TH
import AesonUtils

newtype TwitchCondition =
  TwitchCondition
    { twitchConditionBroadcasterUserId :: Text
    }

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchCondition") ''TwitchCondition

data TwitchTransport =
  TwitchTransport
    { twitchTransportMethod :: Text
    , twitchTransportCallback :: Text
    }

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchTransport") ''TwitchTransport

data TwitchSubscription =
  TwitchSubscription
    { twitchSubscriptionId :: Text
    , twitchSubscriptionStatus :: Text
    , twitchSubscriptionType :: Text
    , twitchSubscriptionVersion :: Text
    , twitchSubscriptionCost :: Text
    , twitchSubscriptionCondition :: TwitchCondition
    , twitchSubscriptionTransport :: TwitchTransport
    , twitchSubscriptionCreatedAt :: Text
    }

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchSubscription") ''TwitchSubscription

data TwitchEventDetails =
  TwitchEventDetails
    { twitchEventDetailsUserId :: Text
    , twitchEventDetailsUserLogin :: Text
    , twitchEventDetailsUserName :: Text
    , twitchEventDetailsBroadcasterUserId :: Text
    , twitchEventDetailsBroadcasterUserLogin :: Text
    , twitchEventDetailsBroadcasterUserName :: Text
    }

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchEventDetails") ''TwitchEventDetails

data TwitchEvent =
  TwitchEvent
    { twitchEventSubscription :: TwitchSubscription
    , twitchEventEvent :: TwitchEventDetails
    }

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchEvent") ''TwitchEvent
