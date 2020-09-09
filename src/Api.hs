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
import Data.Time (getCurrentTime)
import Database (insertToken_)
import Database.Beam.Sqlite (runBeamSqlite)
import Database.SQLite.Simple (close, open)
import Database.Table.Token (TokenType (UserToken))
import GHC.Generics (Generic)
import Servant
  ( Headers,
    JSON,
    NoContent (NoContent),
    QueryParam,
    StdMethod (GET),
    Verb,
    addHeader,
    (:>),
  )
import Servant.API (Get)
import Servant.API.Generic (ToServantApi, genericApi, (:-))
import Servant.API.Header (Header)
import Servant.Server (Application)
import Servant.Server.Generic (AsServerT, genericServe)
import System.Random (randomRIO)
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
    _oauth2callback :: route :- "oauth2" :> "callback" :> QueryParam "code" T.Text :> QueryParam "state" T.Text :> QueryParam "scope" T.Text :> Get '[JSON] ()
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
              liftIO $ runBeamSqlite conn $ insertToken_ UserToken code utcTime
            Nothing -> error "..."
          liftIO $ close conn
          -- TODO:
          -- - GET /subscribers
          -- - GET /followers
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
