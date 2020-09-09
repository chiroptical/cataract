{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Table.Token where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Beam
  ( Beamable,
    C,
    FromBackendRow (fromBackendRow),
    Generic,
    Identity,
    Table (PrimaryKey, primaryKey),
  )
import Database.Beam.Backend
  ( HasSqlValueSyntax (sqlValueSyntax),
    autoSqlValueSyntax,
  )
import Database.Beam.Sqlite.Connection (Sqlite)

data TokenType
  = UserToken
  deriving (Eq, Show, Read, Ord, Enum, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be TokenType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite TokenType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data Token = Token
  { tokenType :: TokenType,
    bearer :: T.Text,
    issueTime :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data TokenT f = Token_
  { _tokenType :: C f TokenType,
    _tokenBearer :: C f T.Text,
    _tokenIssueTime :: C f UTCTime
  }
  deriving (Generic, Beamable)

type Token_ = TokenT Identity

deriving instance Eq Token_

deriving instance Show Token_

instance Table TokenT where
  data PrimaryKey TokenT f = TokenId (C f TokenType)
    deriving (Generic, Beamable)
  primaryKey = TokenId . _tokenType
