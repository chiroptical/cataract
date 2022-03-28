{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Borrowed from https://github.com/freckle/yesod-auth-oauth2/blob/main/src/Yesod/Auth/OAuth2/Twitch.hs
-- Want ability to override the 'pluginName'
module Yesod.Auth.OAuth2.MyTwitch
  ( oauth2TwitchScoped
  ) where

import Import.NoFoundation
import Yesod.Auth.OAuth2.Prelude

import qualified Data.Text.Encoding as T

newtype User = User Text

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "user_id"

oauth2TwitchScoped :: YesodAuth m => Text -> [Text] -> Text -> Text -> AuthPlugin m
oauth2TwitchScoped loginWithText scopes clientId clientSecret =
  authOAuth2 loginWithText oauth2 $ \manager token -> do
    (User userId, userResponse) <- authGetProfile
      loginWithText
      manager
      token
      "https://id.twitch.tv/oauth2/validate"

    pure Creds { credsPlugin = loginWithText
               , credsIdent  = userId
               , credsExtra  = setExtra token userResponse
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
