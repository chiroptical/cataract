{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Request.Twitch.Followers where

import Data.Aeson
import Import.NoFoundation hiding (GET)
import Request.Twitch

newtype Followers = Followers
  { followersToId :: Text
  }
  deriving (Show)

data FollowersPayload = FollowersPayload
  deriving (Generic)

instance ToJSON FollowersPayload

newtype FollowersResponse = FollowersResponse
  { followersResponseTotal :: Int
  }
  deriving (Show)

instance FromJSON FollowersResponse where
  parseJSON = withObject "FollowersResponse" $ \v ->
    FollowersResponse
      <$> v .: "total"

instance TwitchRequest Followers where
  type TwitchPayload Followers = FollowersPayload
  type TwitchResponse Followers = FollowersResponse
  twitchRequestMethod = GET
  twitchRequestPath = "users/follows"
  twitchQueryParams Followers{..} = [("to_id", followersToId)]
