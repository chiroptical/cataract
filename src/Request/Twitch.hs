{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Request.Twitch where

import Control.Lens
import Network.Wreq
import Data.Kind (Type)
import Import.NoFoundation hiding (responseBody)
import Data.Text.Encoding qualified as Text
import Data.Twitch

data TwitchMethod = GET | POST
  deriving Show

class TwitchRequest endpoint where
  type TwitchPayload endpoint :: Type
  type TwitchResponse endpoint :: Type

  -- | This is converted to a request type in 'Network.Wreq'
  twitchRequestMethod :: TwitchRequest endpoint => TwitchMethod

  twitchBaseUrl :: TwitchRequest endpoint => String
  twitchBaseUrl = "https://api.twitch.tv/helix"

  -- | The resource to request, e.g. 'users/follows'
  twitchRequestPath :: TwitchRequest endpoint => String

  -- | The query parameters, combined with 'defaults' to build options
  twitchQueryParams :: TwitchRequest endpoint => endpoint -> [(Text, Text)]

  twitchRequestOptions :: TwitchRequest endpoint => endpoint -> Options
  twitchRequestOptions endpoint =
    let queryParams = twitchQueryParams endpoint
     in foldl' (\acc (name, val) -> acc & param name .~ [val]) defaults queryParams

  twitchRequest ::
    ( MonadIO m
    , MonadThrow m
    , TwitchRequest endpoint
    , ToJSON (TwitchPayload endpoint)
    , FromJSON (TwitchResponse endpoint)
    ) =>
    endpoint ->
    Text ->
    TwitchCredentials ->
    TwitchPayload endpoint ->
    m (Either TwitchError (TwitchResponse endpoint))
  twitchRequest endpoint clientId TwitchCredentials {..} payload = do
    let opts = twitchRequestOptions endpoint
                & header "Authorization" .~ ["Bearer " <> Text.encodeUtf8 twitchCredentialsAccessToken]
                & header "Client-Id" .~ [Text.encodeUtf8 clientId]
        method = twitchRequestMethod @endpoint
        url = twitchBaseUrl @endpoint <> "/" <> twitchRequestPath @endpoint
    response <- liftIO $ customPayloadMethodWith (show method) opts url (toJSON payload)
    decoded <- asJSON @_ @(TwitchResponse endpoint) response
    pure $ Right (decoded ^. responseBody)
