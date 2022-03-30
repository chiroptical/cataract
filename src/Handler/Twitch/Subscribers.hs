module Handler.Twitch.Subscribers where

import Import
import Request.Twitch
import Request.Twitch.Subscribers
import Handler.Twitch.Utils

getSubscribersR :: Handler Text
getSubscribersR = do
  TwitchSettings {..} <- appTwitchSettings <$> getsYesod appSettings
  eTwitchCredentials <- getTwitchCredentials
  case eTwitchCredentials of
    Left e -> sendStatusJSON status401 e
    Right creds -> do
      let subscriberRequest = Subscribers twitchSettingsStreamerId
      eResponse <- twitchRequest subscriberRequest twitchSettingsClientId creds SubscribersPayload
      case eResponse of
        -- TODO: Probably should just send exceptions directly to the client?
        Left e -> sendStatusJSON status500 e
        Right SubscribersResponse {..} -> pure $ tshow subscribersResponseTotal 
