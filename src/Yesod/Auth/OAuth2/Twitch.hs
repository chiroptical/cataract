{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yesod.Auth.OAuth2.Twitch (
  oauth2Twitch,
  oauth2TwitchScoped,
) where

import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Yesod.Auth.OAuth2.Prelude

data TwitchUser = TwitchUser
  { twitchUserId           :: Text
  , twitchUserAccessToken  :: Text
  , twitchUserRefreshToken :: Text
  }

instance Show TwitchUser where
  show TwitchUser {..} = "TwitchUser { twitchUserId = " <> Text.unpack twitchUserId <> ", ... }"

instance FromJSON TwitchUser where
  parseJSON =
    withObject "TwitchUser" $
      \o ->
        TwitchUser
          <$> o .: "user_id"
          <*> o .: "access_token"
          <*> o .: "refresh_token"

pluginName :: Text
pluginName = "twitch"

defaultScopes :: [Text]
defaultScopes = ["channel:read:subscriptions", "user:read:follows", "user:read:subscriptions"]

oauth2Twitch :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Twitch = oauth2TwitchScoped defaultScopes

oauth2TwitchScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2TwitchScoped scopes clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (TwitchUser {..}, userResponse) <-
      authGetProfile
        pluginName
        manager
        token
        "https://id.twitch.tv/oauth2/validate"

    pure
      Creds
        { credsPlugin = pluginName
        , credsIdent = twitchUserId
        , credsExtra = setExtra token userResponse
        }
  where
    oauth2 =
      OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = Just clientSecret
        , oauthOAuthorizeEndpoint =
            "https://id.twitch.tv/oauth2/authorize"
              `withQuery` [ scopeParam " " scopes
                          , -- TODO: bring in yesod-auth-oauth2 0.7.0.1 via nix
                            -- yesod-auth-oauth2 0.6.3.4 doesn't have a redirect_uri
                            -- parameter, if we update we can move this
                            -- TODO: This should not be hardcoded...
                            ("redirect_uri", "http://localhost:3000/auth/page/twitch/callback")
                          ]
        , oauthAccessTokenEndpoint =
            "https://id.twitch.tv/oauth2/token"
              `withQuery` [ -- TODO: bring in yesod-auth-oauth2 0.7.0.1 via nix
                            -- yesod-auth-oauth2 0.6.3.4 doesn't have a redirect_uri
                            -- parameter, if we update we can move this
                            -- TODO: This should not be hardcoded...
                            ("redirect_uri", "http://localhost:3000/auth/page/twitch/callback")
                          , ("client_id", Text.encodeUtf8 clientId)
                          , ("client_secret", Text.encodeUtf8 clientSecret)
                          ]
        , oauthCallback = Nothing
        }
