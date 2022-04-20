{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Twitch.Webhook where

import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.Aeson (decodeStrict)
import Data.ByteArray.Encoding qualified as BA
import Data.ByteString qualified as BS
import Data.Conduit.List qualified as CL
import Data.Conduit.Text qualified as CT
import Data.EventKind (EventKind (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Twitch.Webhook
import Database.Esqueleto.Experimental (from, select)
import Handler.Twitch.Webhook.Sql
import Import hiding (requestHeaders)
import Network.Wai (Request (..), responseLBS)
import Request.Twitch
import Request.Twitch.AppAccess
import Request.Twitch.SubscribeToEvent

toBase16 :: HMAC SHA256 -> ByteString
toBase16 = BA.convertToBase @(Digest SHA256) @ByteString BA.Base16 . hmacGetDigest

postAdminReplayWebhookR :: EventId -> Handler ()
postAdminReplayWebhookR eventId = do
    mEvent <- runDB $ get eventId
    case mEvent of
        Nothing -> sendStatusJSON status404 ("Unable to find event" :: Text)
        Just Event{} ->
            runDB $
                insert_
                    Queue
                        { queueEventId = eventId
                        , queueCompleted = False
                        }
    redirect AdminWebhooksR

getAdminWebhooksR :: Handler Html
getAdminWebhooksR = do
    theQueue <- runDB . select . from $ getRecentNEventsOffQueue 30
    defaultLayout $ do
        setTitle "Admin Queue"
        $(widgetFile "adminWebhooks")

postTwitchWebhookR :: Handler ()
postTwitchWebhookR = do
    TwitchSettings{..} <- appTwitchSettings <$> getsYesod appSettings
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
        then case Map.lookup "Twitch-Eventsub-Message-Type" requestHeaderMap of
            Just "webhook_callback_verification" -> do
                let mTwitchChallenge = decodeStrict @TwitchChallenge bodyBs
                case mTwitchChallenge of
                    Nothing -> sendStatusJSON status401 ("Unable to decode challenge" :: Text)
                    Just TwitchChallenge{..} -> do
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
                    Just
                        TwitchEvent
                            { twitchEventSubscription = TwitchSubscription{..}
                            , twitchEventEvent = event
                            } -> do
                            case (twitchSubscriptionType, event) of
                                (FollowEventType, BaseEventDetails TwitchEventDetailsBase{..}) ->
                                    runDB $ do
                                        eventId <- insert $ Event{eventKind = NewFollowerKind}
                                        insert_ $
                                            FollowerEvent
                                                { followerEventEventId = eventId
                                                , followerEventTwitchUserName = twitchEventDetailsBaseUserName
                                                }
                                        insert_ $
                                            Queue
                                                { queueEventId = eventId
                                                , queueCompleted = False
                                                }
                                (SubscribeEventType, BaseEventDetails TwitchEventDetailsBase{..}) ->
                                    runDB $ do
                                        eventId <- insert $ Event{eventKind = NewSubscriberKind}
                                        insert_ $
                                            SubscriberEvent
                                                { subscriberEventEventId = eventId
                                                , subscriberEventTwitchUserName = twitchEventDetailsBaseUserName
                                                }
                                        insert_ $
                                            Queue
                                                { queueEventId = eventId
                                                , queueCompleted = False
                                                }
                                (CheerEventType, CheerEventDetails TwitchEventDetailsCheer{..}) ->
                                    runDB $ do
                                        eventId <- insert $ Event{eventKind = NewCheerKind}
                                        insert_ $
                                            CheerEvent
                                                { cheerEventEventId = eventId
                                                , cheerEventTwitchUserName = twitchEventDetailsCheerUserName
                                                , cheerEventBits = twitchEventDetailsCheerBits
                                                }
                                        insert_ $
                                            Queue
                                                { queueEventId = eventId
                                                , queueCompleted = False
                                                }
                                (RaidEventType, RaidEventDetails TwitchEventDetailsRaid{..}) ->
                                    runDB $ do
                                        eventId <- insert $ Event{eventKind = NewRaidKind}
                                        insert_ $
                                            RaidEvent
                                                { raidEventEventId = eventId
                                                , raidEventTwitchUserName = twitchEventDetailsRaidFromBroadcasterUserName
                                                , raidEventViewers = twitchEventDetailsRaidViewers
                                                }
                                        insert_ $
                                            Queue
                                                { queueEventId = eventId
                                                , queueCompleted = False
                                                }
                                _ -> sendStatusJSON status401 $ "Event " <> tshow twitchSubscriptionType <> " is not implemented"
            Just _ -> sendStatusJSON status401 ("Unrecognized event type" :: Text)
            Nothing -> sendStatusJSON status401 ("Unable to find event type header" :: Text)
        else sendStatusJSON status401 ("Unable to validate request" :: Text)

getAdminWebhookHandler :: TwitchEventType -> Handler ()
getAdminWebhookHandler eventType = do
    AppSettings{appDevelopment, appTwitchSettings} <- getsYesod appSettings
    let TwitchSettings{..} = appTwitchSettings
    -- Subscribe to webhooks for streamer, unless we are in the development
    -- environment where we don't have SSL termination
    unless appDevelopment $ do
        eAppAccessResponse <-
            twitchRequestNoCreds
                (AppAccess twitchSettingsClientId twitchSettingsClientSecret)
                AppAccessPayload
        case eAppAccessResponse of
            Left _ -> sendStatusJSON status401 ("Unable to subscribe to event" :: Text)
            Right AppAccessResponse{..} -> do
                let buildEventRequest =
                        twitchRequest SubscribeToEvent twitchSettingsClientId (AccessToken appAccessAccessToken)
                            . buildSubscribeToEventPayload appTwitchSettings
                eEventResponse <- buildEventRequest eventType
                case eEventResponse of
                    Left _ -> sendStatusJSON status401 ("Unable to subscribe to event" :: Text)
                    Right res -> do
                        -- Twitch docs say there should only be one result...
                        let [item] = subscribeToEventResponseData res
                            mkWebhookSubscription eventData =
                                WebhookSubscription
                                    { webhookSubscriptionEventId = tshow $ subscribeToEventDataId eventData
                                    , webhookSubscriptionType = tshow $ subscribeToEventDataType eventData
                                    }
                        runDB . insert_ $ mkWebhookSubscription item

getAdminWebhooksSubscribeFollowR :: Handler ()
getAdminWebhooksSubscribeFollowR = getAdminWebhookHandler FollowEventType

getAdminWebhooksSubscribeSubscribeR :: Handler ()
getAdminWebhooksSubscribeSubscribeR = getAdminWebhookHandler SubscribeEventType

getAdminWebhooksSubscribeCheerR :: Handler ()
getAdminWebhooksSubscribeCheerR = getAdminWebhookHandler CheerEventType

getAdminWebhooksSubscribeRaidR :: Handler ()
getAdminWebhooksSubscribeRaidR = getAdminWebhookHandler RaidEventType
