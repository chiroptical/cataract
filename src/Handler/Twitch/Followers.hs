module Handler.Twitch.Followers where

import Handler.Twitch.Utils
import Import
import Request.Twitch
import Request.Twitch.Followers

getFollowersR :: Handler Text
getFollowersR = do
  TwitchSettings {..} <- appTwitchSettings <$> getsYesod appSettings
  eTwitchCredentials <- getTwitchCredentials
  case eTwitchCredentials of
    Left e -> sendStatusJSON status401 e
    Right creds -> do
      let followersRequest = Followers twitchSettingsStreamerId
      eResponse <- twitchRequest followersRequest twitchSettingsClientId creds FollowersPayload
      case eResponse of
        -- TODO: Probably should just send exceptions directly to the client?
        Left e                       -> sendStatusJSON status500 e
        Right FollowersResponse {..} -> pure $ tshow followersResponseTotal
