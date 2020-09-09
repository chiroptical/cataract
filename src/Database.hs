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
    dbModification,
    defaultDbSettings,
    insertExpressions,
    modifyTableFields,
    setEntityName,
    tableModification,
    val_,
    withDbModification,
  )
import Database.Beam.Sqlite
  ( SqliteM,
    insertReturning,
    runInsertReturningList,
  )
import Database.SQLite.Simple (Connection, execute_)
import Database.Table.Token (TokenT (..), TokenType (..), Token_)

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
    \, issue_time TEXT NOT NULL \
    \)"

insertToken_ :: TokenType -> T.Text -> UTCTime -> SqliteM Token_
insertToken_ tokenType bearer utcTime = do
  [token_] <-
    runInsertReturningList $
      insertReturning (_tableToken db) $
        insertExpressions [Token_ (val_ tokenType) (val_ bearer) (val_ utcTime)]
  pure token_
