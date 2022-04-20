{-# LANGUAGE DeriveGeneric #-}

module Data.Twitch where

import Import.NoFoundation

data TwitchError
    = UnableToFetchCredentials
    deriving (Show, Generic)

instance ToJSON TwitchError
