{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
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
    Generic,
    Identity,
    Table (PrimaryKey, primaryKey),
  )

data Token = Token
  { tokenType :: T.Text,
    bearer :: T.Text,
    refresh :: T.Text,
    issueTime :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

toToken :: Token_ -> Token
toToken Token_ {..} = Token _tokenType _tokenBearer _tokenRefresh _tokenIssueTime

data TokenT f = Token_
  { _tokenType :: C f T.Text,
    _tokenBearer :: C f T.Text,
    _tokenRefresh :: C f T.Text,
    _tokenIssueTime :: C f UTCTime
  }
  deriving (Generic, Beamable)

type Token_ = TokenT Identity

deriving instance Eq Token_

deriving instance Show Token_

instance Table TokenT where
  data PrimaryKey TokenT f = TokenId (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey = TokenId . _tokenType
