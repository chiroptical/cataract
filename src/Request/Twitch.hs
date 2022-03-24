{-# LANGUAGE TypeFamilies #-}

module Request.Twitch where

import Network.Wreq
import Data.Kind (Type)
import Data.Text (Text)
import Import.NoFoundation

data TwitchMethod = GET | POST
  deriving Show

class TwitchRequest endpoint where
  data TwitchPayload endpoint :: Type
  data TwitchResponse endpoint :: Type

  -- | This is converted to a request type in 'Network.Wreq'
  twitchRequestMethod :: endpoint -> TwitchMethod

  twitchBaseUrl :: endpoint -> String
  twitchBaseUrl = const "https://api.twitch.tv/helix"

  -- | The resource to request, e.g. 'users/follows'
  twitchRequestPath :: endpoint -> String

  -- | The query parameters, combined with 'defaults' to build options
  twitchQueryParams :: endpoint -> [(Text, Text)]

  -- | Grab these from the database or refresh the token
  twitchCredentials :: MonadIO m => endpoint -> m Text

  -- Notes: use 'customMethodWith :: String -> Options -> String -> IO (Response L.ByteString)'
