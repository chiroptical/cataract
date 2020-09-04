{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module TwitchApi
  ( twitchClientAuthorize
  ) where

import           Control.Monad.IO.Class  (MonadIO)
import           Data.Proxy              (Proxy (Proxy))
import qualified Data.Text               as T
import           Database.Beam           (MonadIO (liftIO))
import           GHC.Generics            (Generic)
import           GHC.IO                  (throwIO)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant                 ((:>), JSON, QueryParam)
import           Servant.API             (Get, QueryParam', Required, Strict)
import           Servant.API.Generic     ((:-), ToServantApi, genericApi)
import           Servant.Client          (BaseUrl (BaseUrl), mkClientEnv,
                                          runClientM)
import           Servant.Client.Core     (Scheme (Https))
import           Servant.Client.Generic  (AsClientT, genericClientHoist)

type RequiredQueryParam = QueryParam' '[ Required, Strict]

data OAuthRoutes route =
  OAuthRoutes
    { _authorize :: route :- "oauth2" :> "authorize" :> RequiredQueryParam "client_id" T.Text :> RequiredQueryParam "redirect_uri" T.Text :> RequiredQueryParam "response_type" T.Text :> RequiredQueryParam "scope" T.Text :> Get '[ JSON] ()
    }
  deriving (Generic)

api :: Proxy (ToServantApi OAuthRoutes)
api = genericApi (Proxy :: Proxy OAuthRoutes)

clientRoutes :: OAuthRoutes (AsClientT IO)
clientRoutes =
  genericClientHoist
    (\x -> do
       manager' <- newManager tlsManagerSettings
       let env = mkClientEnv manager' (BaseUrl Https "id.twitch.tv" 443 "")
       runClientM x env >>= either throwIO return)

twitchClientAuthorize :: T.Text -> T.Text -> T.Text -> T.Text -> IO ()
twitchClientAuthorize = _authorize clientRoutes
