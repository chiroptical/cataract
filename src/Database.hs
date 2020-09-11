{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Beam
  ( Database,
    DatabaseSettings,
    Generic,
    TableEntity,
    all_,
    dbModification,
    defaultDbSettings,
    guard_,
    insertExpressions,
    modifyTableFields,
    runSelectReturningOne,
    select,
    setEntityName,
    tableModification,
    val_,
    withDbModification,
    (==.),
  )
import Database.Beam.Sqlite
  ( SqliteM,
    insertReturning,
    runInsertReturningList,
  )
import Database.SQLite.Simple (Connection, execute_)
import Database.Table.Token (TokenT (..), Token_)

newtype Db f = Db
  { _tableToken :: f (TableEntity TokenT)
  }
  deriving (Generic, Database be)

db :: DatabaseSettings be Db
db =
  defaultDbSettings
    `withDbModification` dbModification
      { _tableToken =
          setEntityName "tokens"
            <> modifyTableFields
              tableModification
                { _tokenType = "type",
                  _tokenBearer = "bearer",
                  _tokenRefresh = "refresh",
                  _tokenIssueTime = "issue_time"
                }
      }

makeTablesIfNotExists :: Connection -> IO ()
makeTablesIfNotExists conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS tokens \
    \( type TEXT NOT NULL UNIQUE PRIMARY KEY \
    \, bearer TEXT NOT NULL \
    \, refresh TEXT NOT NULL \
    \, issue_time TEXT NOT NULL \
    \)"

data Token = UserToken | AuthorizationToken deriving (Show)

upsertToken_ :: Token -> T.Text -> T.Text -> UTCTime -> SqliteM Token_
upsertToken_ token bearer refresh utcTime = undefined

insertToken_ :: Token -> T.Text -> T.Text -> UTCTime -> SqliteM Token_
insertToken_ token bearer refresh utcTime = do
  [token_] <-
    runInsertReturningList $
      insertReturning (_tableToken db) $
        insertExpressions [Token_ (val_ (T.pack . show $ token)) (val_ bearer) (val_ refresh) (val_ utcTime)]
  pure token_

selectToken :: Token -> SqliteM (Maybe Token_)
selectToken token =
  runSelectReturningOne $
    select $ do
      token_ <- all_ (_tableToken db)
      guard_ (_tokenType token_ ==. val_ (T.pack . show $ token))
      pure token_
