{-# LANGUAGE TemplateHaskell #-}

module Data.Twitch.Webhook where

import AesonUtils
import Data.Aeson.TH
import Import.NoFoundation

newtype TwitchCondition =
  TwitchCondition
    { twitchConditionBroadcasterUserId :: Text
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchCondition") ''TwitchCondition

data TwitchTransport =
  TwitchTransport
    { twitchTransportMethod   :: Text
    , twitchTransportCallback :: Text
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchTransport") ''TwitchTransport

data TwitchEventType
  = FollowEvent
  | SubscribeEvent
  | GiftedSubscribeEvent
  | CheerEvent
  | RaidEvent
  | UserAuthorizationGrantEvent
  deriving Show

instance FromJSON TwitchEventType where
  parseJSON (String s) = case s of
                           "channel.follow" -> pure FollowEvent
                           "channel.subscribe" -> pure SubscribeEvent
                           "channel.subscription.gift" -> pure GiftedSubscribeEvent
                           "channel.cheer" -> pure CheerEvent
                           "channel.raid" -> pure RaidEvent
                           "user.authorization.grant" -> pure UserAuthorizationGrantEvent
                           _ -> mzero
  parseJSON _ = mzero

instance ToJSON TwitchEventType where
  toJSON = \case
    FollowEvent                 -> "channel.follow"
    SubscribeEvent              -> "channel.subscribe"
    GiftedSubscribeEvent        -> "channel.subscription.gift"
    CheerEvent                  -> "channel.cheer"
    RaidEvent                   -> "channel.raid"
    UserAuthorizationGrantEvent -> "user.authorization.grant"

data TwitchSubscription =
  TwitchSubscription
    { twitchSubscriptionId        :: Text
    , twitchSubscriptionStatus    :: Text
    , twitchSubscriptionType      :: TwitchEventType
    , twitchSubscriptionVersion   :: Text
    , twitchSubscriptionCost      :: Int
    , twitchSubscriptionCondition :: TwitchCondition
    , twitchSubscriptionTransport :: TwitchTransport
    , twitchSubscriptionCreatedAt :: Text
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchSubscription") ''TwitchSubscription

data TwitchEventDetails =
  TwitchEventDetails
    { twitchEventDetailsUserId               :: Text
    , twitchEventDetailsUserLogin            :: Text
    , twitchEventDetailsUserName             :: Text
    , twitchEventDetailsBroadcasterUserId    :: Text
    , twitchEventDetailsBroadcasterUserLogin :: Text
    , twitchEventDetailsBroadcasterUserName  :: Text
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchEventDetails") ''TwitchEventDetails

data TwitchEvent =
  TwitchEvent
    { twitchEventSubscription :: TwitchSubscription
    , twitchEventEvent        :: TwitchEventDetails
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchEvent") ''TwitchEvent

data TwitchChallenge =
  TwitchChallenge
    { twitchChallengeChallenge    :: Text
    , twitchChallengeSubscription :: TwitchSubscription
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchChallenge") ''TwitchChallenge
