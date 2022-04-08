module Handler.Twitch.Webhook where

import Crypto.Hash             (Digest)
import Crypto.Hash.Algorithms  (SHA256)
import Crypto.MAC.HMAC         (HMAC (..), hmac)
import Data.Aeson              (decodeStrict)
import Data.ByteArray.Encoding qualified as BA
import Data.ByteString         qualified as BS
import Data.Conduit.List       qualified as CL
import Data.Conduit.Text       qualified as CT
import Data.Map                qualified as Map
import Data.Text               qualified as T
import Data.Text.Encoding      qualified as T
import Data.Text.Lazy          qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Twitch.Webhook
import Import                  hiding (requestHeaders)
import Network.Wai             (Request (..), responseLBS)

toBase16 :: HMAC SHA256 -> ByteString
toBase16 = BA.convertToBase @(Digest SHA256) @ByteString BA.Base16 . hmacGetDigest

postTwitchWebhookR :: Handler ()
postTwitchWebhookR = do
  TwitchSettings {..} <- appTwitchSettings <$> getsYesod appSettings
  request <- waiRequest
  body <- T.concat <$> runConduit (rawRequestBody .| CT.decode CT.utf8 .| CL.consume)
  let requestHeaderMap = Map.fromList $ requestHeaders request
      bodyBs = T.encodeUtf8 body
      mHmacMessage =
        (\x y z -> x <> y <> z)
          <$> Map.lookup "Twitch-Eventsub-Message-Id" requestHeaderMap
          <*> Map.lookup "Twitch-Eventsub-Message-Timestamp" requestHeaderMap
          <*> pure bodyBs
      mHmac = toBase16 . hmac @_ @_ @SHA256 (T.encodeUtf8 twitchSettingsClientSecret) <$> mHmacMessage
      mTwitchSignature = BS.stripPrefix "sha256=" =<< Map.lookup "Twitch-Eventsub-Message-Signature" requestHeaderMap
      validHmac = fromMaybe False $ (==) <$> mHmac <*> mTwitchSignature
  if validHmac
     then
        case Map.lookup "Twitch-Eventsub-Message-Type" requestHeaderMap of
          Just "webhook_callback_verification" -> do
            let mTwitchChallenge = decodeStrict @TwitchChallenge bodyBs
            case mTwitchChallenge of
              Nothing -> sendStatusJSON status401 ("Unable to decode challenge" :: Text)
              Just TwitchChallenge {..} -> do
                let response =
                        responseLBS
                          status200
                          [("Content-Type", "text/plain")]
                          (LT.encodeUtf8 . LT.fromStrict $ twitchChallengeChallenge)
                sendWaiResponse response
          Just "notification" -> do
              let mTwitchEvent = decodeStrict @TwitchEvent bodyBs
              case mTwitchEvent of
                Nothing -> sendStatusJSON status401 ("Unable to decode event" :: Text)
                -- TODO: Store the event
                Just te@TwitchEvent {} -> do
                  print te
          Just _ -> sendStatusJSON status401 ("Unrecognized event type" :: Text)
          Nothing -> sendStatusJSON status401 ("Unable to find event type header" :: Text)
      else
        sendStatusJSON status401 ("Unable to validate request" :: Text)
