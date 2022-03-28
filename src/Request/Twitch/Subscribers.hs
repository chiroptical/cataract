{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Request.Twitch.Subscribers where

import Request.Twitch
import Import.NoFoundation hiding (GET)
import Data.Aeson

newtype Subscribers = Subscribers
  { subscribersBroadcasterId :: Text
  }
  deriving Show

data SubscribersPayload = SubscribersPayload
  deriving Generic

instance ToJSON SubscribersPayload

newtype SubscribersResponse = SubscribersResponse
  { subscribersResponseTotal :: Int
  }
  deriving Show

instance FromJSON SubscribersResponse where
  parseJSON = withObject "SubscribersResponse" $ \v -> SubscribersResponse
      <$> v .: "total"

instance TwitchRequest Subscribers where
  type TwitchPayload Subscribers = SubscribersPayload
  type TwitchResponse Subscribers = SubscribersResponse
  twitchRequestMethod = GET
  twitchRequestPath = "subscriptions"
  twitchQueryParams Subscribers {..} = [("broadcaster_id", subscribersBroadcasterId)]
