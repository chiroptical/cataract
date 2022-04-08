module Data.Event.Subscriber where

import Import.NoFoundation

data Subscriber =
  Subscriber
    { twitchUserName       :: Text
    , subscriptionDuration :: Int
    , wasGifted            :: Bool
    }
    deriving Generic

instance ToJSON Subscriber
