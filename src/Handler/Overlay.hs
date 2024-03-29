{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Overlay where

import Database.Esqueleto.Experimental qualified as Db
import Encryption (decryptText, runEncryptM)
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

getOverlayR :: Handler Html
getOverlayR = do
  AppSettings {..} <- getsYesod appSettings
  let TwitchSettings {..} = appTwitchSettings
  -- The overlay is powered by streamer's Twitch credentials
  -- If they aren't present we can't display anything
  mTwitchCredentials <- runDB . Db.selectOne $ queryCredentialsFromIdent twitchSettingsStreamerId
  Entity _ TwitchCredentials {..} <-
    maybe
      (sendStatusJSON status401 ("streamer must log in" :: Text))
      pure
      mTwitchCredentials
  eAccessToken <- liftIO $ runEncryptM appEncryptionSettings $ decryptText twitchCredentialsAccessToken
  case eAccessToken of
    Left _ -> sendStatusJSON status401 ("Unable to retrieve access token" :: Text)
    Right accessToken -> do
      -- Get the follower and subscriber count for the initial overlay content
      (followerCount, subscriberCount) :: (Text, Text) <- do
        let followersRequest = Followers twitchSettingsStreamerId
        eFollowerResponse <-
          twitchRequest
            followersRequest
            twitchSettingsClientId
            (AccessToken accessToken)
            FollowersPayload
        let subscriberRequest = Subscribers twitchSettingsStreamerId
        eSubscriberResponse <-
          twitchRequest
            subscriberRequest
            twitchSettingsClientId
            (AccessToken accessToken)
            SubscribersPayload
        case (eFollowerResponse, eSubscriberResponse) of
          (Right (FollowersResponse x), Right (SubscribersResponse y)) -> pure (tshow x, tshow y)
          _ -> pure ("No response", "from Twitch")
      emptyLayout $ do
        setTitle "Overlay"
        $(widgetFile "overlay")
