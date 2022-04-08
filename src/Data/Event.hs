module Data.Event where

import Data.Event.Follower
import Data.Event.Subscriber
import Import.NoFoundation

data Event =
    NewFollower Follower
  | NewSubscriber Subscriber
  deriving Generic

instance ToJSON Event
