{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Config (Config (..))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAlphaNum)
import Data.Functor (void)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database (Metric (..), NamedMetric (Followers, Subscribers), NamedToken (AuthorizationCode, UserCode), Token (..), selectMetric, selectToken, tokenRefresh, upsertMetric, upsertToken)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite (Entity (Entity, entityVal), runSqlPool)
import GHC.Generics (Generic)
import Lucid (ToHtml (toHtml, toHtmlRaw), body_, charset_, href_, link_, meta_, rel_, script_, src_, title_)
import Lucid.Base (Html, renderBS)
import Lucid.Html5 (head_)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Media ((//), (/:))
import Servant (Accept (..), Headers, JSON, MimeRender (..), NoContent (NoContent), QueryParam, Raw, ServerError (errBody), StdMethod (GET), Verb, addHeader, err404, serveDirectoryFileServer, (:>))
import Servant.API (Get)
import Servant.API.Generic (AsApi, ToServant, toServant, (:-))
import Servant.API.Header (Header)
import Servant.Client (BaseUrl (BaseUrl), mkClientEnv, runClientM)
import Servant.Client.Core.Reexport (Scheme (Http))
import Servant.Client.Generic (AsClientT, genericClientHoist)
import Servant.Server (Application)
import Servant.Server.Generic (AsServerT, genericServe)
import System.Random (randomRIO)
import TwitchApi.Auth (RefreshResponse (..), TokenResponse (..), twitchAuthToken, twitchRefreshToken)
import TwitchApi.Helix (User, twitchFollowers, twitchSubscribers, twitchUsers)
import Url (Url (Url), urlEncode)

genRandomState :: IO T.Text
genRandomState = T.pack <$> go 32 []
  where
    go :: Int -> String -> IO String
    go 0 acc = pure acc
    go n acc = do
      char <- randomRIO ('0', 'z')
      if isAlphaNum char
        then go (n - 1) (char : acc)
        else go n acc

data HTMLLucid

data Overlay = Overlay
  deriving (Generic, ToJSON, FromJSON)

showText :: Show a => a -> T.Text
showText = T.pack . show

instance ToHtml Overlay where
  toHtmlRaw = toHtml
  toHtml _ = do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      link_ [rel_ "stylesheet", href_ "/static/styles.css"]
      title_ "Overlay"
    body_ (script_ [src_ "/static/app.js"] ("" :: T.Text))

instance Accept HTMLLucid where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
  mimeRender _ = renderBS . toHtml

instance MimeRender HTMLLucid (Html a) where
  mimeRender _ = renderBS

type NoContentWithLocation = Headers (Header "Location" T.Text ': '[]) NoContent

type Redirect302 = Verb 'GET 302 '[JSON] NoContentWithLocation

data Routes route = Routes
  { _authorize :: route :- "authorize" :> Redirect302,
    _oauth2callback :: route :- "oauth2" :> "callback" :> QueryParam "code" T.Text :> QueryParam "state" T.Text :> QueryParam "scope" T.Text :> Get '[JSON] (),
    _exchange :: route :- "exchange" :> Get '[JSON] (),
    _refresh :: route :- "refresh" :> Get '[JSON] (),
    _subscribers :: route :- "subscribers" :> Get '[JSON] Int,
    _users :: route :- "users" :> Get '[JSON] [User],
    _followers :: route :- "followers" :> Get '[JSON] Int,
    _overlay :: route :- "overlay" :> Get '[JSON, HTMLLucid] Overlay,
    _static :: route :- "static" :> ToServant StaticContent AsApi,
    _update :: route :- "update" :> Get '[JSON] ()
  }
  deriving (Generic)

data StaticContent route = StaticContent
  { _staticContent :: route :- Raw
  }
  deriving (Generic)

staticContentHandler :: MonadIO m => StaticContent (AsServerT m)
staticContentHandler =
  StaticContent
    { _staticContent = serveDirectoryFileServer "./static"
    }

-- TODO:
-- - IO should probably be a `Sem r a` or Transformer equivalent?
handlers :: MonadIO m => Config -> Pool SqlBackend -> Routes (AsServerT m)
handlers config@Config {..} pool =
  Routes
    { _authorize =
        -- TODO:
        -- - Need to provide a CSRF safe `state` to the url
        return (addHeader url NoContent :: NoContentWithLocation),
      -- TODO:
      -- - Compare state to previous state (see below)
      -- - Store the scope as `Text` in the database
      -- - Should we POST the token back to the server?
      _oauth2callback =
        -- \mCode mState mScope -> do
        \mCode _ _ -> case mCode of
          Just code -> liftIO . runStdoutLoggingT . void $ runSqlPool (upsertToken UserCode code "...") pool
          Nothing -> pure (),
      -- TODO:
      -- - GET /subscribers
      -- - GET /followers
      _exchange = do
        let redirectUri =
              "http://localhost:"
                <> (T.pack . show) portNumber
                <> "/oauth2/callback"
        mToken <- liftIO . runStdoutLoggingT $ runSqlPool (selectToken UserCode) pool
        void $ case mToken of
          Just Entity {entityVal = Token {..}} ->
            ( do
                TokenResponse {..} <- liftIO $ twitchAuthToken clientId clientSecret tokenCode "authorization_code" redirectUri
                void . liftIO . runStdoutLoggingT $ runSqlPool (upsertToken AuthorizationCode access_token refresh_token) pool
            )
          Nothing -> pure (),
      _refresh = do
        mToken <- liftIO . runStdoutLoggingT $ runSqlPool (selectToken AuthorizationCode) pool
        void $ case mToken of
          Just Entity {entityVal = Token {..}} ->
            ( do
                RefreshResponse {..} <- liftIO $ twitchRefreshToken clientId clientSecret tokenRefresh "refresh_token"
                void . liftIO . runStdoutLoggingT $ runSqlPool (upsertToken AuthorizationCode access_token refresh_token) pool
            )
          Nothing -> pure (),
      _subscribers = liftIO . runStdoutLoggingT $ do
        mSubs <- runSqlPool (selectMetric Subscribers) pool
        pure $ case mSubs of
          Just Entity {entityVal = Metric {..}} -> metricNumber
          Nothing -> 0,
      -- TODO
      -- - May need to rethink how _users endpoint works
      _users = do
        liftIO $ refreshAuthorizationCode config
        mToken <- liftIO . runStdoutLoggingT $ runSqlPool (selectToken AuthorizationCode) pool
        case mToken of
          Just Entity {entityVal = Token {..}} -> liftIO $ twitchUsers streamerName tokenCode clientId
          Nothing -> pure [],
      _followers = liftIO . runStdoutLoggingT $ do
        mFols <- runSqlPool (selectMetric Database.Followers) pool
        pure $ case mFols of
          Just Entity {entityVal = Metric {..}} -> metricNumber
          Nothing -> 0,
      _overlay = pure Overlay,
      _static =
        toServant staticContentHandler,
      _update = liftIO $ do
        -- TODO:
        -- - Likely, we need this whole process in a `catch`
        refreshAuthorizationCode config
        mToken <- runStdoutLoggingT $ runSqlPool (selectToken AuthorizationCode) pool
        case mToken of
          Just Entity {entityVal = Token {..}} -> do
            subs <- twitchSubscribers (T.pack . show $ streamerId) tokenCode clientId
            fols <- twitchFollowers (T.pack . show $ streamerId) tokenCode clientId
            void . runStdoutLoggingT $ runSqlPool (upsertMetric Subscribers subs >> upsertMetric Database.Followers fols) pool
          Nothing -> throwError $ err404 {errBody = "AuthorizationCode unavailable during /update"}
    }
  where
    url =
      urlEncode $
        Url
          "https://id.twitch.tv/oauth2/authorize"
          [ ("client_id", clientId),
            ( "redirect_uri",
              "http://localhost:"
                <> (T.pack . show) portNumber
                <> "/oauth2/callback"
            ),
            ("response_type", "code"),
            ("scope", requiredScopes)
          ]

throwError :: ServerError -> IO ()
throwError = error "not implemented"

app :: Config -> Pool SqlBackend -> Application
app config pool = genericServe (handlers config pool)

clientRoutes :: Config -> Routes (AsClientT IO)
clientRoutes Config {..} =
  genericClientHoist
    ( \x -> do
        manager' <- newManager defaultManagerSettings
        let env = mkClientEnv manager' (BaseUrl Http "localhost" portNumber "")
        runClientM x env >>= either throwIO return
    )

refreshAuthorizationCode :: Config -> IO ()
refreshAuthorizationCode config = _refresh (clientRoutes config)

getSubscribers :: Config -> IO Int
getSubscribers config = _subscribers (clientRoutes config)

getFollowers :: Config -> IO Int
getFollowers config = do
  n <- _followers (clientRoutes config)
  pure n
