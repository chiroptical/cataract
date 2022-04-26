{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestImport (
  module TestImport,
  module X,
) where

import Application (makeFoundation, makeLogWare)
import ClassyPrelude as X hiding (Handler, delete, deleteBy)
import Database.Esqueleto.Experimental (rawExecute)
import Database.Persist as X hiding (get)
import Database.Persist.Sql (SqlPersistM, rawSql, runSqlPersistMPool, unSingle)
import Database.Persist.SqlBackend (getEscapeRawNameFunction)
import Foundation as X
import Model as X
import Test.Hspec as X
import Text.Shakespeare.Text (st)
import Yesod.Auth as X
import Yesod.Core.Unsafe (fakeHandlerGetLogger)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)
import Yesod.Test as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
  app <- getTestYesod
  liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
  app <- getTestYesod
  fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  settings <-
    loadYamlSettings
      ["config/test-settings.yml", "config/settings.yml"]
      []
      useEnv
  foundation <- makeFoundation settings
  wipeDB foundation
  logWare <- liftIO $ makeLogWare foundation
  pure (foundation, logWare)

-- TODO: Need to finish this function!
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
  tables <- getTables
  sqlBackend <- ask
  let escapedTables = map (getEscapeRawNameFunction sqlBackend) tables
      query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
  rawExecute query []

getTables :: DB [Text]
getTables = do
  tables <-
    rawSql
      [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_type = 'BASE TABLE';
    |]
      []

  pure $ map unSingle tables

{- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
 being set in test-settings.yaml, which enables dummy authentication in
 Foundation.hs
-}

-- TODO: Dummy auth is enabled in tests, but I need to figure out how to
-- authenticate a test user. Check
-- https://hackage.haskell.org/package/yesod-test-1.6.13/docs/Yesod-Test.html#v:setRequestBody
authenticateAs :: Entity TwitchUser -> YesodExample App ()
authenticateAs (Entity _ TwitchUser {..}) = do
  request $ do
    setMethod "POST"
    -- TODO: construct a 'Creds' object via 'setRequestBody'
    setUrl $ AuthR $ PluginR "dummy" []

{- | Create a user.  The dummy email entry helps to confirm that foreign-key
 checking is switched off in wipeDB for those database backends which need it.
-}
createUser :: Text -> YesodExample App (Entity TwitchUser)
createUser ident =
  runDB $
    insertEntity
      TwitchUser
        { twitchUserIdent = ident
        , twitchUserLogin = ident
        }
