{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Overlay where

import Import
import Database.Esqueleto.Experimental qualified as Db
import Request.Twitch.Sql (queryCredentialsFromIdent)
import Request.Twitch.Followers
import Request.Twitch.Subscribers
import Request.Twitch (twitchRequest)

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
  TwitchSettings {..} <- appTwitchSettings <$> getsYesod appSettings
  mTwitchCredentials <- runDB . Db.selectOne $ queryCredentialsFromIdent twitchSettingsStreamerId
  (followerCount, subscriberCount) :: (Text, Text) <-
    case mTwitchCredentials of
      Nothing -> pure ("Subscriber credentials", "are missing")
      Just (Entity _ creds) -> do
        let followersRequest = Followers twitchSettingsStreamerId
        eFollowerResponse <- twitchRequest followersRequest twitchSettingsClientId creds FollowersPayload
        let subscriberRequest = Subscribers twitchSettingsStreamerId
        eSubscriberResponse <- twitchRequest subscriberRequest twitchSettingsClientId creds SubscribersPayload
        case (eFollowerResponse, eSubscriberResponse) of
          (Right (FollowersResponse x), Right (SubscribersResponse y)) -> pure (tshow x, tshow y)
          _ -> pure ("No response", "from Twitch")
  emptyLayout $ do
    setTitle "Overlay"
    $(widgetFile "overlay")
