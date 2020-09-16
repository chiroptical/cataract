{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TwitchApi.Auth where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.IO (throwIO)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (JSON, Post, QueryParam', Required, Strict, (:>))
import Servant.API.Generic (ToServantApi, genericApi, (:-))
import Servant.Client
  ( BaseUrl (BaseUrl),
    mkClientEnv,
    runClientM,
  )
import Servant.Client.Core (Scheme (Https))
import Servant.Client.Generic (AsClientT, genericClientHoist)

data TokenResponse = TokenResponse
  { access_token :: T.Text,
    refresh_token :: T.Text,
    expires_in :: Int,
    scope :: [T.Text],
    token_type :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RefreshResponse = RefreshResponse
  { access_token :: T.Text,
    refresh_token :: T.Text,
    scope :: [T.Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type RequiredQP = QueryParam' '[Required, Strict]

data OAuthRoutes route = OAuthRoutes
  { _token :: route :- "oauth2" :> "token" :> RequiredQP "client_id" T.Text :> RequiredQP "client_secret" T.Text :> RequiredQP "code" T.Text :> RequiredQP "grant_type" T.Text :> RequiredQP "redirect_uri" T.Text :> Post '[JSON] TokenResponse,
    _refresh :: route :- "oauth2" :> "token" :> RequiredQP "client_id" T.Text :> RequiredQP "client_secret" T.Text :> RequiredQP "refresh_token" T.Text :> RequiredQP "grant_type" T.Text :> Post '[JSON] RefreshResponse
  }
  deriving (Generic)

api :: Proxy (ToServantApi OAuthRoutes)
api = genericApi (Proxy :: Proxy OAuthRoutes)

clientRoutes :: OAuthRoutes (AsClientT IO)
clientRoutes =
  genericClientHoist
    ( \x -> do
        manager' <- newManager tlsManagerSettings
        let env = mkClientEnv manager' (BaseUrl Https "id.twitch.tv" 443 "")
        runClientM x env >>= either throwIO return
    )

-- TODO:
-- - Consider wrapping these in newtypes for clarity?
twitchAuthToken :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> IO TokenResponse
twitchAuthToken = _token clientRoutes

twitchRefreshToken :: T.Text -> T.Text -> T.Text -> T.Text -> IO RefreshResponse
twitchRefreshToken = _refresh clientRoutes
