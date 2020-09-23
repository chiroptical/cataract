{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Text as T
import Database.Persist (Entity)
import Database.Persist.Sqlite (PersistQueryRead (selectFirst), PersistUniqueWrite (upsert), SqlBackend, runMigration, (=.), (==.))
import qualified Database.Persist.TH as PTH

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
Token
  name T.Text
  code T.Text
  refresh T.Text
  UniqueToken name

Metric
  name T.Text
  number Int
  UniqueMetric name
|]

data NamedToken = UserCode | AuthorizationCode deriving (Show, Eq)

data NamedMetric = Subscribers | Followers deriving (Show, Eq)

deriving instance Show Token

deriving instance Eq Token

showText :: Show a => a -> T.Text
showText = T.pack . show

makeTables :: MonadIO m => ReaderT SqlBackend m ()
makeTables = runMigration migrateAll

upsertToken :: MonadIO m => NamedToken -> T.Text -> T.Text -> ReaderT SqlBackend m (Entity Token)
upsertToken name bearer refresh = upsert record [TokenCode =. bearer, TokenRefresh =. refresh]
  where
    record = Token (showText name) bearer refresh

selectToken :: MonadIO m => NamedToken -> ReaderT SqlBackend m (Maybe (Entity Token))
selectToken name = selectFirst [TokenName ==. token] []
  where
    token = showText name

upsertMetric :: MonadIO m => NamedMetric -> Int -> ReaderT SqlBackend m (Entity Metric)
upsertMetric name number = upsert record [MetricNumber =. number]
  where
    record = Metric (showText name) number

selectMetric :: MonadIO m => NamedMetric -> ReaderT SqlBackend m (Maybe (Entity Metric))
selectMetric name = selectFirst [MetricName ==. token] []
  where
    token = showText name
