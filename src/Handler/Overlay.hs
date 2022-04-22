{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Overlay where

import Database.Esqueleto.Experimental qualified as Db
import Import
import Request.Twitch (
  AccessToken (..),
  TwitchRequest (..),
  twitchRequest,
 )
import Request.Twitch.Followers
import Request.Twitch.Sql (queryCredentialsFromIdent)
import Request.Twitch.Subscribers

emptyLayout :: Yesod site => WidgetFor site () -> HandlerFor site Html
emptyLayout w = do
  p <- widgetToPageContent w
  withUrlRenderer
    [hamlet|
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

{- | Documentation

 - svg.js: https://svgjs.dev/docs/3.1

 | Next

 - Set up the EventSource handler in JS and start sending messages
-}
getOverlayR :: Handler Html
getOverlayR = do
  TwitchSettings {..} <- appTwitchSettings <$> getsYesod appSettings
  -- The overlay is powered by streamer's Twitch credentials
  -- If they aren't present we can't display anything
  mTwitchCredentials <- runDB . Db.selectOne $ queryCredentialsFromIdent twitchSettingsStreamerId
  Entity _ TwitchCredentials {..} <-
    maybe
      (sendStatusJSON status401 ("streamer must log in" :: Text))
      pure
      mTwitchCredentials
  let accessToken = AccessToken twitchCredentialsAccessToken
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
