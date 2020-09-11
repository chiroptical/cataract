{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Config (Config (..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isAlphaNum)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Data.Time (UTCTime (..), diffTimeToPicoseconds, getCurrentTime, picosecondsToDiffTime, utctDay, utctDayTime)
import Database (Token (..), insertToken_, selectToken)
import Database.Beam.Sqlite (runBeamSqlite)
import Database.SQLite.Simple (close, open)
import Database.Table.Token (TokenT (..))
import GHC.Generics (Generic)
import Servant (Headers, JSON, NoContent (NoContent), QueryParam, StdMethod (GET), Verb, addHeader, (:>))
import Servant.API (Get)
import Servant.API.Generic (ToServantApi, genericApi, (:-))
import Servant.API.Header (Header)
import Servant.Server (Application)
import Servant.Server.Generic (AsServerT, genericServe)
import System.Random (randomRIO)
import TwitchApi (RefreshResponse (..), TokenResponse (..), twitchAuthToken, twitchRefreshToken)
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

type NoContentWithLocation = Headers (Header "Location" T.Text ': '[]) NoContent

type Redirect302 = Verb 'GET 302 '[JSON] NoContentWithLocation

data Routes route = Routes
  { _authorize :: route :- "authorize" :> Redirect302,
    _oauth2callback :: route :- "oauth2" :> "callback" :> QueryParam "code" T.Text :> QueryParam "state" T.Text :> QueryParam "scope" T.Text :> Get '[JSON] (),
    _exchange :: route :- "exchange" :> Get '[JSON] (),
    _refresh :: route :- "refresh" :> Get '[JSON] ()
  }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

-- TODO:
-- - IO should probably be a `Sem r a` or Transformer equivalent?
handlers :: MonadIO m => Config -> Routes (AsServerT m)
handlers Config {..} =
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
        \mCode _ _ -> do
          conn <- liftIO $ open "test.db"
          utcTime <- liftIO getCurrentTime
          void $ case mCode of
            Just code ->
              -- TODO:
              -- - Should probably replace "..." with a Maybe in the database
              liftIO $ runBeamSqlite conn $ insertToken_ UserToken code "..." utcTime
            -- TODO:
            -- - This is terrible...
            Nothing -> error "..."
          liftIO $ close conn,
      -- TODO:
      -- - GET /subscribers
      -- - GET /followers
      _exchange = do
        let redirectUri =
              "http://localhost:"
                <> (T.pack . show) portNumber
                <> "/oauth2/callback"
        conn <- liftIO $ open "test.db"
        utcTime <- liftIO getCurrentTime
        mCode <- liftIO $ runBeamSqlite conn (selectToken UserToken)
        void $ case mCode of
          Just Token_ {..} -> do
            TokenResponse {..} <- liftIO $ twitchAuthToken clientId clientSecret _tokenBearer "authorization_code" redirectUri
            let oldPicoseconds = diffTimeToPicoseconds (utctDayTime utcTime)
            let expiresInPicoseconds = (fromIntegral expires_in) * 1000000000000
            let newDiffTime = picosecondsToDiffTime $ oldPicoseconds + expiresInPicoseconds
            let newUtcTime = UTCTime (utctDay utcTime) newDiffTime
            liftIO $ runBeamSqlite conn $ insertToken_ AuthorizationToken access_token refresh_token newUtcTime
          Nothing -> error "..."
        liftIO $ close conn,
      _refresh = do
        conn <- liftIO $ open "test.db"
        utcTime <- liftIO getCurrentTime
        mCode <- liftIO $ runBeamSqlite conn (selectToken AuthorizationToken)
        void $ case mCode of
          Just Token_ {..} -> do
            RefreshResponse {..} <- liftIO $ twitchRefreshToken clientId clientSecret _tokenRefresh "refresh_token"
            let oldPicoseconds = diffTimeToPicoseconds (utctDayTime utcTime)
            let expiresInPicoseconds = 3600 * 1000000000000
            let newDiffTime = picosecondsToDiffTime $ oldPicoseconds + expiresInPicoseconds
            let newUtcTime = UTCTime (utctDay utcTime) newDiffTime
            liftIO $ runBeamSqlite conn $ insertToken_ AuthorizationToken access_token refresh_token newUtcTime
          Nothing -> error "..."
        liftIO $ close conn
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

app :: Config -> Application
app = genericServe . handlers
