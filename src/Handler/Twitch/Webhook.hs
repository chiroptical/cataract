module Handler.Twitch.Webhook where

import Import hiding (waiRequest, requestHeaders)
import Data.Twitch.Webhook
import Yesod.Core (waiRequest)
import Network.Wai (Request(..), responseLBS)
import Data.Map qualified as Map
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Crypto.MAC.HMAC (hmac, HMAC (..))
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Hash (Digest)
import Data.ByteString qualified as BS
import Data.Conduit.Text qualified as CT
import Data.Conduit.List qualified as CL
import Data.ByteArray.Encoding qualified as BA

postTwitchWebhookR :: Handler ()
postTwitchWebhookR = do
  TwitchSettings {..} <- appTwitchSettings <$> getsYesod appSettings
  request <- waiRequest
  body <- T.concat <$> runConduit (rawRequestBody .| CT.decode CT.utf8 .| CL.consume)
  print body
  let requestHeaderMap = Map.fromList $ requestHeaders request
      mHmacMessage =
        (\x y z -> x <> y <> z)
          <$> Map.lookup "Twitch-Eventsub-Message-Id" requestHeaderMap
          <*> Map.lookup "Twitch-Eventsub-Message-Timestamp" requestHeaderMap
          <*> pure (T.encodeUtf8 body)
      -- TODO: These are equal, but the types are incompatible
      mHmac = BA.convertToBase @(Digest SHA256) @ByteString BA.Base64 . hmacGetDigest . hmac @_ @_ @SHA256 (T.encodeUtf8 twitchSettingsClientSecret) <$> mHmacMessage
      mTwitchSignature = BS.stripPrefix "sha256=" =<< Map.lookup "Twitch-Eventsub-Message-Signature" requestHeaderMap
  print mHmac
  print mTwitchSignature
  case ((==) <$> mHmac <*> mTwitchSignature, Map.lookup "Twitch-Eventsub-Message-Type" requestHeaderMap) of
    (Nothing, _) -> sendStatusJSON status401 ("One..." :: Text) -- TODO: better error message
    (_, Nothing) -> sendStatusJSON status401 ("Two..." :: Text) -- TODO: better error message
    (Just False, _) -> sendStatusJSON status401 ("Three..." :: Text) -- TODO: better error message
    (Just True, Just eventType) -> do
      case eventType of
        "webhook_callback_verification" -> do
          TwitchChallenge {..} <- requireCheckJsonBody @_ @TwitchChallenge
          let response =
                  responseLBS
                    status200
                    [("Content-Type", "text/plain")]
                    (LT.encodeUtf8 . LT.fromStrict $ twitchChallengeChallenge)
          sendWaiResponse response
        "notification" -> do
          te@TwitchEvent {} <- requireCheckJsonBody @_ @TwitchEvent
          print te
        _ -> sendStatusJSON status401 ("Four..." :: Text)
