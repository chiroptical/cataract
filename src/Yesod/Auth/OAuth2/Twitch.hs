{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DerivingStrategies #-}

module Yesod.Auth.OAuth2.Twitch (
  oauth2Twitch,
  oauth2TwitchScoped,
) where

import qualified Data.Text.Encoding        as Text
import           Yesod.Auth.OAuth2.Prelude

newtype TwitchUser = TwitchUser
  { twitchUserId           :: Text
  }
  deriving stock Show

instance FromJSON TwitchUser where
  parseJSON =
    withObject "TwitchUser" $
      \o ->
        TwitchUser
          <$> o .: "user_id"

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
    oauth2 = OAuth2
          { oauth2ClientId          = clientId
          , oauth2ClientSecret      = Just clientSecret
          , oauth2AuthorizeEndpoint = "https://id.twitch.tv/oauth2/authorize"
                                        `withQuery` [ scopeParam " " scopes
                                                    ]
          , oauth2TokenEndpoint     = "https://id.twitch.tv/oauth2/token"
                                        `withQuery` [ ("client_id", Text.encodeUtf8 clientId)
                                                    , ("client_secret", Text.encodeUtf8 clientSecret)
                                                    ]
          , oauth2RedirectUri       = Nothing
          }
