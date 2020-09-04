{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Config                 (Config (..))
import           Control.Applicative    (Applicative (liftA2))
import           Control.Monad          (replicateM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Char              (isAlphaNum, toLower)
import           Data.Proxy             (Proxy (Proxy))
import qualified Data.Text              as T
import           Data.Time              (getCurrentTime)
import           Database               (insertToken_)
import           Database.Beam.Sqlite   (runBeamSqlite)
import           Database.SQLite.Simple (close, open)
import           Database.Table.Token   (TokenType (UserToken))
import           GHC.Generics           (Generic)
import           Servant                ((:>), Headers, JSON,
                                         NoContent (NoContent), QueryParam,
                                         ServerError (errHeaders),
                                         StdMethod (GET), Verb, addHeader,
                                         err302, throwError)
import           Servant.API            (Get)
import           Servant.API.Generic    ((:-), ToServantApi, genericApi)
import           Servant.API.Header     (Header)
import           Servant.Server         (Application)
import           Servant.Server.Generic (AsServerT, genericServe)
import           System.Random          (getStdGen, randomRIO)
import           TwitchApi              (twitchClientAuthorize)

genRandomState :: IO T.Text
genRandomState = T.pack <$> go 32 []
  where
    go :: Int -> [Char] -> IO [Char]
    go 0 acc = pure acc
    go n acc = do
      char <- randomRIO ('0', 'z')
      if isAlphaNum char
        then go (n - 1) (char : acc)
        else go n acc

data Routes route =
  Routes
    { _authorize :: route :- "authorize" :> Get '[ JSON] ()
    , _oauth2callback :: route :- "oauth2" :> "callback" :> QueryParam "code" T.Text :> QueryParam "state" T.Text :> QueryParam "scope" T.Text :> Get '[ JSON] ()
    }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

-- TODO:
-- - IO should probably be a `Sem r a` or Transformer equivalent?
handlers :: MonadIO m => Config -> Routes (AsServerT m)
handlers Config {..} =
  Routes
    { _authorize
        -- TODO:
        -- - Parameters like client_id, port, scope should come from `Reader Config` effect
        -- - This could probably be defined as another Servant API and exposed as a client
        -- - Need to provide a CSRF safe state to this redirect
       =
        liftIO $
        twitchClientAuthorize
          clientId
          ("http://localhost:" <>
           (T.pack . show) portNumber <> "/oauth2/callback")
          "code"
          requiredScopes
      -- TODO:
      -- - Compare state to previous state (see below)
      -- - Store the scope as `Text` in the database
    , _oauth2callback =
        \mCode mState mScope -> do
          conn <- liftIO $ open "test.db"
          utcTime <- liftIO $ getCurrentTime
          case mCode of
            Just code ->
              liftIO $ runBeamSqlite conn $ insertToken_ UserToken code utcTime
          liftIO $ close conn
          -- TODO:
          -- - GET /subscribers
          -- - GET /followers
    }

app :: Config -> Application
app = genericServe . handlers
