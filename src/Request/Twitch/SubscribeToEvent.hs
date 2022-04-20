{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Request.Twitch.SubscribeToEvent where

import AesonUtils
import Data.Aeson
import Data.Aeson.TH
import Data.Twitch.Webhook
import Data.UUID
import Import.NoFoundation hiding (POST)
import Request.Twitch

newtype BaseCondition = BaseCondition
    { baseConditionBroadcasterUserId :: Text
    }
    deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "BaseCondition") ''BaseCondition

newtype RaidCondition = RaidCondition
    { raidConditionToBroadcasterUserId :: Text
    }
    deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "RaidCondition") ''RaidCondition

data Condition
    = BaseC BaseCondition
    | RaidC RaidCondition
    deriving (Show)

deriveJSON (defaultOptions{sumEncoding = UntaggedValue}) ''Condition

data Transport = Transport
    { transportMethod :: Text
    , transportCallback :: Text
    , transportSecret :: Text
    }
    deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "Transport") ''Transport

data SubscribeToEventPayload = SubscribeToEventPayload
    { subscribeToEventPayloadType :: TwitchEventType
    , subscribeToEventPayloadVersion :: Text
    , subscribeToEventPayloadCondition :: Condition
    , subscribeToEventPayloadTransport :: Transport
    }
    deriving (Show)

deriveJSON (jsonDeriveSnakeCaseDropPrefix "SubscribeToEventPayload") ''SubscribeToEventPayload

data SubscribeToEventData = SubscribeToEventData
    { subscribeToEventDataId :: UUID
    , subscribeToEventDataType :: TwitchEventType
    }

deriveJSON (jsonDeriveSnakeCaseDropPrefix "SubscribeToEventData") ''SubscribeToEventData

newtype SubscribeToEventResponse = SubscribeToEventResponse
    { subscribeToEventResponseData :: [SubscribeToEventData]
    }

deriveJSON (jsonDeriveSnakeCaseDropPrefix "SubscribeToEventResponse") ''SubscribeToEventResponse

data SubscribeToEvent = SubscribeToEvent

instance TwitchRequest SubscribeToEvent where
    type TwitchPayload SubscribeToEvent = SubscribeToEventPayload
    type TwitchResponse SubscribeToEvent = SubscribeToEventResponse
    twitchRequestMethod = POST
    twitchRequestPath = "eventsub/subscriptions"
    twitchQueryParams _ = []

buildSubscribeToEventPayload :: TwitchSettings -> TwitchEventType -> SubscribeToEventPayload
buildSubscribeToEventPayload TwitchSettings{..} twitchEvType =
    SubscribeToEventPayload
        { subscribeToEventPayloadType = twitchEvType
        , subscribeToEventPayloadVersion = "1"
        , subscribeToEventPayloadCondition = case twitchEvType of
            RaidEventType -> RaidC $ RaidCondition twitchSettingsStreamerId
            _ -> BaseC $ BaseCondition twitchSettingsStreamerId
        , subscribeToEventPayloadTransport =
            Transport
                { transportMethod = "webhook"
                , transportCallback = twitchSettingsCallback
                , transportSecret = twitchSettingsClientSecret
                }
        }
