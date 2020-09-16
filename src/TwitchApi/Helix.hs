{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TwitchApi.Helix where

import Control.Applicative (optional)
import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.IO (throwIO)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (Get, JSON, QueryParam', Required, Strict, (:>))
import Servant.API (Header')
import Servant.API.Generic (ToServantApi, genericApi, (:-))
import Servant.Client
  ( BaseUrl (BaseUrl),
    mkClientEnv,
    runClientM,
  )
import Servant.Client.Core (Scheme (Https))
import Servant.Client.Generic (AsClientT, genericClientHoist)

data Response a = Response
  { entries :: [a],
    paginationCursor :: Maybe T.Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "response" $ \json -> do
    subs <- json .: "data"
    pagination' <- optional (json .: "pagination")
    cursor <- case pagination' of
      Just pagination -> pagination .: "cursor"
      Nothing -> pure Nothing
    return $ Response subs cursor

data Subscriber = Subscriber
  { broadcaster_id :: T.Text,
    broadcaster_name :: T.Text,
    is_gift :: Bool,
    tier :: T.Text,
    plan_name :: T.Text,
    user_id :: T.Text,
    user_name :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data User = User
  { id :: T.Text,
    display_name :: T.Text,
    view_count :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Followers = Followers
  { total :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type RequiredQP = QueryParam' '[Required, Strict]

type RequiredHeader = Header' '[Required, Strict]

data OAuthRoutes route = OAuthRoutes
  { --TODO
    -- - Optional user_id parameter, how to deal with this?
    _subscribers :: route :- "subscriptions" :> RequiredQP "broadcaster_id" T.Text :> RequiredHeader "Authorization" T.Text :> RequiredHeader "Client-Id" T.Text :> Get '[JSON] (Response Subscriber),
    _users :: route :- "users" :> RequiredQP "login" T.Text :> RequiredHeader "Authorization" T.Text :> RequiredHeader "Client-Id" T.Text :> Get '[JSON] (Response User),
    _followers :: route :- "users" :> "follows" :> RequiredQP "to_id" T.Text :> RequiredHeader "Authorization" T.Text :> RequiredHeader "Client-Id" T.Text :> Get '[JSON] Followers
  }
  deriving (Generic)

api :: Proxy (ToServantApi OAuthRoutes)
api = genericApi (Proxy :: Proxy OAuthRoutes)

clientRoutes :: OAuthRoutes (AsClientT IO)
clientRoutes =
  genericClientHoist
    ( \x -> do
        manager' <- newManager tlsManagerSettings
        let env = mkClientEnv manager' (BaseUrl Https "api.twitch.tv" 443 "/helix")
        runClientM x env >>= either throwIO return
    )

-- TODO:
-- - Consider wrapping these in newtypes for clarity?
--
-- Subscriber count includes streamer, subtract 1
twitchSubscribers :: T.Text -> T.Text -> T.Text -> IO Int
twitchSubscribers broadcasterId token clientId = do
  response <- _subscribers clientRoutes broadcasterId ("Bearer " <> token) clientId
  return . subtract 1 . length $ entries response

twitchUsers :: T.Text -> T.Text -> T.Text -> IO [User]
twitchUsers login token clientId = do
  response <- _users clientRoutes login ("Bearer " <> token) clientId
  return $ entries response

twitchFollowers :: T.Text -> T.Text -> T.Text -> IO Followers
twitchFollowers login token clientId = _followers clientRoutes login ("Bearer " <> token) clientId
