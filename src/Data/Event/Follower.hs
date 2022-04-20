module Data.Event.Follower where

import Import.NoFoundation

newtype Follower = Follower
  { twitchUserName :: Text
  }
  deriving (Generic)

instance ToJSON Follower
