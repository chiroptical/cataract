{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TwitchApi where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.IO (throwIO)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (JSON, QueryParam', Required, Strict, (:>))
import Servant.API (Get)
import Servant.API.Generic (ToServantApi, genericApi, (:-))
import Servant.Client
  ( BaseUrl (BaseUrl),
    mkClientEnv,
    runClientM,
  )
import Servant.Client.Core (Scheme (Https))
import Servant.Client.Generic (AsClientT, genericClientHoist)

type RequiredQP = QueryParam' '[Required, Strict]

newtype OAuthRoutes route = OAuthRoutes
  { _authorize :: route :- "oauth2" :> "authorize" :> RequiredQP "client_id" T.Text :> RequiredQP "redirect_uri" T.Text :> RequiredQP "response_type" T.Text :> RequiredQP "scope" T.Text :> Get '[JSON] ()
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

twitchAuthorize :: T.Text -> T.Text -> T.Text -> T.Text -> IO ()
twitchAuthorize = _authorize clientRoutes
