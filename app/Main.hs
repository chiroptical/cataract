{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Api (app)
import Config (Config (..))
import Control.Monad.Logger (runStdoutLoggingT)
import Database (makeTables)
import Database.Persist.Sqlite (createSqlitePool, runSqlPool)
import Network.Wai.Handler.Warp (run)
import Path.IO (resolveFile')
import YamlParse.Applicative
  ( readConfigFile,
  )

main :: IO ()
main = do
  configPath <- resolveFile' "./config.yaml"
  config' <- readConfigFile configPath :: IO (Maybe Config)
  -- -- TODO
  -- -- - If Nothing, we should stop...
  case config' of
    Just config@Config {..} ->
      -- TODO: Should have connection pool to feed to app
      do
        pool <- runStdoutLoggingT $ createSqlitePool ("WAL=off " <> databaseFileName) 5
        runStdoutLoggingT $ runSqlPool makeTables pool
        run 8081 (app config pool)
    Nothing -> pure ()
