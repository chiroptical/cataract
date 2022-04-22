{-# LANGUAGE TemplateHaskell #-}

module Data.Twitch.Webhook where

import AesonUtils
import Data.Aeson.TH
import Import.NoFoundation

data TwitchTransport = TwitchTransport
  { twitchTransportMethod :: Text
  , twitchTransportCallback :: Text
  }
  deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchTransport") ''TwitchTransport

data TwitchEventType
  = FollowEventType
  | SubscribeEventType
  | CheerEventType
  | RaidEventType
  deriving (Show)

instance FromJSON TwitchEventType where
  parseJSON (String s) = case s of
    "channel.follow" -> pure FollowEventType
    "channel.subscribe" -> pure SubscribeEventType
    "channel.cheer" -> pure CheerEventType
    "channel.raid" -> pure RaidEventType
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON TwitchEventType where
  toJSON = \case
    FollowEventType -> "channel.follow"
    SubscribeEventType -> "channel.subscribe"
    CheerEventType -> "channel.cheer"
    RaidEventType -> "channel.raid"

data TwitchSubscription = TwitchSubscription
  { twitchSubscriptionId :: Text
  , twitchSubscriptionStatus :: Text
  , twitchSubscriptionType :: TwitchEventType
  , twitchSubscriptionVersion :: Text
  , twitchSubscriptionCost :: Int
  , -- Note: raids use 'to_broadcaster_user_id', others use 'broadcaster_user_id'
    twitchSubscriptionCondition :: Value
  , twitchSubscriptionTransport :: TwitchTransport
  , twitchSubscriptionCreatedAt :: Text
  }
  deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchSubscription") ''TwitchSubscription

data TwitchEventDetailsBase = TwitchEventDetailsBase
  { twitchEventDetailsBaseUserId :: Text
  , twitchEventDetailsBaseUserLogin :: Text
  , twitchEventDetailsBaseUserName :: Text
  , twitchEventDetailsBaseBroadcasterUserId :: Text
  , twitchEventDetailsBaseBroadcasterUserLogin :: Text
  , twitchEventDetailsBaseBroadcasterUserName :: Text
  }
  deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchEventDetailsBase") ''TwitchEventDetailsBase

data TwitchEventDetailsCheer = TwitchEventDetailsCheer
  { twitchEventDetailsCheerUserId :: Text
  , twitchEventDetailsCheerUserLogin :: Text
  , twitchEventDetailsCheerUserName :: Text
  , twitchEventDetailsCheerBroadcasterUserId :: Text
  , twitchEventDetailsCheerBroadcasterUserLogin :: Text
  , twitchEventDetailsCheerBroadcasterUserName :: Text
  , twitchEventDetailsCheerIsAnonymous :: Bool
  , twitchEventDetailsCheerMessage :: Text
  , twitchEventDetailsCheerBits :: Int
  }
  deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchEventDetailsCheer") ''TwitchEventDetailsCheer

data TwitchEventDetailsRaid = TwitchEventDetailsRaid
  { twitchEventDetailsRaidToBroadcasterUserId :: Text
  , twitchEventDetailsRaidToBroadcasterUserLogin :: Text
  , twitchEventDetailsRaidToBroadcasterUserName :: Text
  , twitchEventDetailsRaidFromBroadcasterUserId :: Text
  , twitchEventDetailsRaidFromBroadcasterUserLogin :: Text
  , twitchEventDetailsRaidFromBroadcasterUserName :: Text
  , twitchEventDetailsRaidViewers :: Int
  }
  deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchEventDetailsRaid") ''TwitchEventDetailsRaid

data TwitchEventDetailsUserLogin = TwitchEventDetailsUserLogin
  { twitchEventDetailsUserLoginClientId :: Text
  , twitchEventDetailsUserLoginUserId :: Text
  , twitchEventDetailsUserLoginUserLogin :: Text
  , twitchEventDetailsUserLoginUserName :: Text
  }
  deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchEventDetailsUserLogin") ''TwitchEventDetailsUserLogin

data TwitchEventDetails
  = RaidEventDetails TwitchEventDetailsRaid
  | CheerEventDetails TwitchEventDetailsCheer
  | BaseEventDetails TwitchEventDetailsBase
  | UserLoginEventDetails TwitchEventDetailsUserLogin
  deriving (Show)

deriveJSON (defaultOptions {sumEncoding = UntaggedValue}) ''TwitchEventDetails

data TwitchEvent = TwitchEvent
  { twitchEventSubscription :: TwitchSubscription
  , twitchEventEvent :: TwitchEventDetails
  }
  deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchEvent") ''TwitchEvent

data TwitchChallenge = TwitchChallenge
  { twitchChallengeChallenge :: Text
  , twitchChallengeSubscription :: TwitchSubscription
  }
  deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "TwitchChallenge") ''TwitchChallenge
