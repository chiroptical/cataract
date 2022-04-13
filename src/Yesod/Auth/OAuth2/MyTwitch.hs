{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- |
--
-- Borrowed from https://github.com/freckle/yesod-auth-oauth2/blob/main/src/Yesod/Auth/OAuth2/Twitch.hs
-- Want ability to override the 'pluginName'
module Yesod.Auth.OAuth2.MyTwitch
  ( oauth2TwitchScoped
  , UserResponse (..)
  ) where

import Import.NoFoundation
import Yesod.Auth.OAuth2.Prelude

import Data.Text.Encoding        qualified as T
import Yesod.Auth.OAuth2

data User = User
  { userTwitchId        :: Text
  , userTwitchLoginName :: Text
  }

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User
      <$> o .: "user_id"
      <*> o .: "login"

-- | NOTE: When you need more customization just bring in 'authOAuth2Widget'
myAuthOAuth2 :: YesodAuth m => Text -> Text -> OAuth2 -> FetchCreds m -> AuthPlugin m
myAuthOAuth2 display name = authOAuth2Widget [whamlet|#{display}|] name

oauth2TwitchScoped :: YesodAuth m => Text -> Text -> [Text] -> Text -> Text -> AuthPlugin m
oauth2TwitchScoped displayText pluginName scopes clientId clientSecret =
  myAuthOAuth2 displayText pluginName oauth2 $ \manager token -> do
    (User {..}, userResponse) <- authGetProfile
      pluginName
      manager
      token
      "https://id.twitch.tv/oauth2/validate"

    pure Creds { credsPlugin = pluginName
               , credsIdent  = userTwitchId
               , credsExtra  = setExtra token userResponse <> [("login", userTwitchLoginName)]
               }
 where
  oauth2 = OAuth2
    { oauth2ClientId          = clientId
    , oauth2ClientSecret      = Just clientSecret
    , oauth2AuthorizeEndpoint = "https://id.twitch.tv/oauth2/authorize"
                                  `withQuery` [scopeParam " " scopes]
    , oauth2TokenEndpoint     = "https://id.twitch.tv/oauth2/token"
                                  `withQuery` [ ("client_id", T.encodeUtf8 clientId)
                                              , ("client_secret", T.encodeUtf8 clientSecret)
                                              ]
    , oauth2RedirectUri       = Nothing
    }

newtype UserResponse =
  UserResponse
    { userResponseScopes :: [Text]
    }

instance FromJSON UserResponse where
  parseJSON = withObject "UserResponse" $ \o -> UserResponse <$> o .: "scopes"
