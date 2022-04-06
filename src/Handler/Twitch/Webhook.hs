module Handler.Twitch.Webhook where

import Import hiding (waiRequest, requestHeaders)
import Data.Twitch.Webhook
import Yesod.Core (waiRequest)
import Network.Wai (Request(..), responseLBS)
import Data.Map qualified as Map
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text

postTwitchWebhookR :: Handler ()
postTwitchWebhookR = do
  headerMap <- Map.fromList . requestHeaders <$> waiRequest
  case Map.lookup "Twitch-Eventsub-Message-Type" headerMap of
    Nothing -> sendStatusJSON status401 ("No..." :: Text) -- TODO: better error message
    Just eventType ->
      case eventType of
        "webhook_callback_verification" -> do
          TwitchChallenge {..} <- requireCheckJsonBody @_ @TwitchChallenge
          let response =
                  responseLBS
                    status200
                    [("Content-Type", "text/plain")]
                    (Text.encodeUtf8 . Text.fromStrict $ twitchChallengeChallenge)
          sendWaiResponse response
        "notification" -> do
          te@TwitchEvent {} <- requireCheckJsonBody @_ @TwitchEvent
          print te
        -- Any other header is unrecognized
        _ -> sendStatusJSON status401 ("No..." :: Text)
