module Data.Event where

import Import.NoFoundation
import Data.Event.Follower
import Data.Event.Subscriber

data Event =
    NewFollower Follower
  | NewSubscriber Subscriber
  deriving Generic

instance ToJSON Event
