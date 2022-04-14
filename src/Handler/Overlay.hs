{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Handler.Overlay where

import Data.Twitch.Webhook
import Database.Esqueleto.Experimental qualified as Db
import Import
import Request.Twitch                  (AccessToken (..), TwitchRequest (..),
                                        twitchRequest)
import Request.Twitch.AppAccess
import Request.Twitch.Followers
import Request.Twitch.Sql              (queryCredentialsFromIdent)
import Request.Twitch.SubscribeToEvent
import Request.Twitch.Subscribers

emptyLayout :: Yesod site => WidgetFor site () -> HandlerFor site Html
emptyLayout w = do
    p <- widgetToPageContent w
    withUrlRenderer [hamlet|
        $newline never
        $doctype 5
        <html>
          <head>
            <title>#{pageTitle p}
            ^{pageHead p}
            <script src="https://cdn.jsdelivr.net/npm/@svgdotjs/svg.js@3.0/dist/svg.min.js">
          <body>
            ^{pageBody p}
        |]

-- | Documentation
--
-- - svg.js: https://svgjs.dev/docs/3.1
--
-- | Next
--
-- - Set up the EventSource handler in JS and start sending messages
getOverlayR :: Handler Html
getOverlayR = do
  AppSettings {appDevelopment, appTwitchSettings} <- getsYesod appSettings
  let TwitchSettings {..} = appTwitchSettings

  -- The overlay is powered by streamer's Twitch credentials
  -- If they aren't present we can't display anything
  mTwitchCredentials <- runDB . Db.selectOne $ queryCredentialsFromIdent twitchSettingsStreamerId
  Entity _ TwitchCredentials {..} <- maybe
        (sendStatusJSON status401 ("streamer must log in" :: Text))
        pure
        mTwitchCredentials
  let accessToken = AccessToken twitchCredentialsAccessToken

  -- Subscribe to webhooks for streamer, unless we are in the development
  -- environment where we don't have SSL termination
  unless appDevelopment $ do
    eAppAccessResponse <-
      -- TODO: Probably should separate out this request in Twitch
      twitchRequestNoCreds
        (AppAccess twitchSettingsClientId twitchSettingsClientSecret)
        AppAccessPayload
    case eAppAccessResponse of
      Left _ -> sendStatusJSON status401 ("Unable to subscribe to follow event" :: Text)
      Right AppAccessResponse {..} -> do
        let buildEventRequest =
              twitchRequest SubscribeToEvent twitchSettingsClientId (AccessToken appAccessAccessToken)
                . buildSubscribeToEventPayload appTwitchSettings
        eFollowEventResponse <- buildEventRequest FollowEventType
        eSubscribeEventResponse <- buildEventRequest SubscribeEventType
        eCheerEventResponse <- buildEventRequest CheerEventType
        eRaidEventResponse <- buildEventRequest RaidEventType
        case (eFollowEventResponse, eSubscribeEventResponse, eCheerEventResponse, eRaidEventResponse) of
          (Left _, _, _, _) -> sendStatusJSON status401 ("Unable to subscribe to follow event" :: Text)
          (_, Left _, _, _) -> sendStatusJSON status401 ("Unable to subscribe to subscribe event" :: Text)
          (_, _, Left _, _) -> sendStatusJSON status401 ("Unable to subscribe to cheer event" :: Text)
          (_, _, _, Left _) -> sendStatusJSON status401 ("Unable to subscribe to raid event" :: Text)
          _ -> pure ()

  -- Get the follower and subscriber count for the initial overlay content
  (followerCount, subscriberCount) :: (Text, Text) <- do
    let followersRequest = Followers twitchSettingsStreamerId
    eFollowerResponse <- twitchRequest followersRequest twitchSettingsClientId accessToken FollowersPayload
    let subscriberRequest = Subscribers twitchSettingsStreamerId
    eSubscriberResponse <- twitchRequest subscriberRequest twitchSettingsClientId accessToken SubscribersPayload
    case (eFollowerResponse, eSubscriberResponse) of
      (Right (FollowersResponse x), Right (SubscribersResponse y)) -> pure (tshow x, tshow y)
      _ -> pure ("No response", "from Twitch")
  emptyLayout $ do
    setTitle "Overlay"
    $(widgetFile "overlay")
