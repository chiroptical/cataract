{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Request.Twitch.Followers where

import Request.Twitch
import Import.NoFoundation hiding (GET)
import Data.Aeson

data Followers = Followers

data FollowersPayload = FollowersPayload
  deriving Generic

instance ToJSON FollowersPayload

newtype FollowersResponse = FollowersResponse
  { followersResponseTotal :: Int
  }
  deriving Show

instance FromJSON FollowersResponse where
  parseJSON = withObject "FollowersResponse" $ \v -> FollowersResponse
      <$> v .: "total"

instance TwitchRequest Followers where
  type TwitchPayload Followers = FollowersPayload
  type TwitchResponse Followers = FollowersResponse
  twitchRequestMethod = GET
  twitchRequestPath = "users/follows"

  -- TODO: hardcoded to chiroptical for now
  twitchQueryParams = [("to_id", "131787842")]
