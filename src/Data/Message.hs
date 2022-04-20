module Data.Message where

import Import.NoFoundation

data NoMatchingEventMessage = NoMatchingEventMessage

instance ToJSON NoMatchingEventMessage where
  toJSON NoMatchingEventMessage =
    object
      [ "kind" .= ("no-matching-event" :: Text)
      ]

data PingMessage = PingMessage

instance ToJSON PingMessage where
  toJSON PingMessage =
    object
      [ "kind" .= ("ping" :: Text)
      ]

newtype FollowMessage = FollowMessage
  { followMessageTwitchUserName :: Text
  }

instance ToJSON FollowMessage where
  toJSON FollowMessage{..} =
    object
      [ "kind" .= ("follow" :: Text)
      , "from" .= followMessageTwitchUserName
      ]

newtype SubscribeMessage = SubscribeMessage
  { subscribeMessageTwitchUserName :: Text
  }

instance ToJSON SubscribeMessage where
  toJSON SubscribeMessage{..} =
    object
      [ "kind" .= ("subscription" :: Text)
      , "from" .= subscribeMessageTwitchUserName
      ]

data CheerMessage = CheerMessage
  { cheerMessageTwitchUserName :: Text
  , cheerMessageBits :: Int
  }

instance ToJSON CheerMessage where
  toJSON CheerMessage{..} =
    object
      [ "kind" .= ("cheer" :: Text)
      , "from" .= cheerMessageTwitchUserName
      , "bits" .= cheerMessageBits
      ]

data RaidMessage = RaidMessage
  { raidMessageTwitchUserName :: Text
  , raidMessageViewers :: Int
  }

instance ToJSON RaidMessage where
  toJSON RaidMessage{..} =
    object
      [ "kind" .= ("raid" :: Text)
      , "from" .= raidMessageTwitchUserName
      , "raiderCount" .= raidMessageViewers
      ]
