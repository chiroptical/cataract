module Handler.Twitch.Subscribers where

import Encryption (decryptText, runEncryptM)
import Handler.Twitch.Utils
import Import
import Request.Twitch
import Request.Twitch.Subscribers

getSubscribersR :: Handler Text
getSubscribersR = do
  AppSettings {..} <- getsYesod appSettings
  let TwitchSettings {..} = appTwitchSettings
  eTwitchCredentials <- getTwitchCredentials
  case eTwitchCredentials of
    Left e -> sendStatusJSON status401 e
    Right TwitchCredentials {..} -> do
      let subscriberRequest = Subscribers twitchSettingsStreamerId
      eAccessToken <- liftIO $ runEncryptM appEncryptionSettings $ decryptText twitchCredentialsAccessToken
      case eAccessToken of
        Left _ -> sendStatusJSON status401 ("Unable to retrieve access token" :: Text)
        Right accessToken -> do
          eResponse <-
            twitchRequest
              subscriberRequest
              twitchSettingsClientId
              (AccessToken accessToken)
              SubscribersPayload
          case eResponse of
            -- TODO: Probably should just send exceptions directly to the client?
            Left e -> sendStatusJSON status500 e
            Right SubscribersResponse {..} -> pure $ tshow subscribersResponseTotal
