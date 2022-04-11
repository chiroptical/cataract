{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Request.Twitch.SubscribeToEvent where

import AesonUtils
import Data.Aeson
import Data.Aeson.TH
import Import.NoFoundation hiding (POST)
import Request.Twitch

newtype BaseCondition =
  BaseCondition
    { baseConditionBroadcasterUserId :: Text
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "BaseCondition") ''BaseCondition

newtype RaidCondition =
  RaidCondition
    { raidConditionToBroadcasterUserId :: Text
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "RaidCondition") ''RaidCondition

data Condition
  = BaseC BaseCondition
  | RaidC RaidCondition
  deriving Show

deriveJSON (defaultOptions {sumEncoding = UntaggedValue}) ''Condition

data Transport =
  Transport
    { transportMethod   :: Text
    , transportCallback :: Text
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "Transport") ''Transport

data SubscribeToEventPayload =
  SubscribeToEventPayload
    { subscribeToEventPayloadType      :: Text
    , subscribeToEventPayloadVersion   :: Text
    , subscribeToEventPayloadCondition :: Condition
    , subscribeToEventPayloadTransport :: Transport
    }
    deriving Show

deriveJSON (jsonDeriveSnakeCaseDropPrefix "SubscribeToEventPayload") ''SubscribeToEventPayload

data SubscribeToEventResponse = SubscribeToEventResponse

data SubscribeToEvent = SubscribeToEvent

instance TwitchRequest SubscribeToEvent where
  type TwitchPayload SubscribeToEvent = SubscribeToEventPayload
  type TwitchResponse SubscribeToEvent = SubscribeToEventResponse
  twitchRequestMethod = POST
  twitchRequestPath = "eventsub/subscriptions"
  twitchQueryParams _ = []
