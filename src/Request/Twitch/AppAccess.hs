{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Request.Twitch.AppAccess where

import Data.Aeson
import Import.NoFoundation hiding (POST)
import Request.Twitch

data AppAccess = AppAccess
    { appAccessClientId :: Text
    , appAccessClientSecret :: Text
    }
    deriving (Show)

data AppAccessPayload = AppAccessPayload
    deriving (Generic)

instance ToJSON AppAccessPayload

newtype AppAccessResponse = AppAccessResponse
    { appAccessAccessToken :: Text
    }
    deriving (Show)

instance FromJSON AppAccessResponse where
    parseJSON = withObject "AppAccessResponse" $ \v ->
        AppAccessResponse
            <$> v .: "access_token"

instance TwitchRequest AppAccess where
    type TwitchPayload AppAccess = AppAccessPayload
    type TwitchResponse AppAccess = AppAccessResponse
    twitchBaseUrl = "https://id.twitch.tv"
    twitchRequestMethod = POST
    twitchRequestPath = "oauth2/token"
    twitchQueryParams AppAccess{..} =
        [ ("client_id", appAccessClientId)
        , ("client_secret", appAccessClientSecret)
        , ("grant_type", "client_credentials")
        ]
