{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Api                             (app)
import           Config                          (Config (..))
import qualified Data.Text                       as T
import           Data.Time                       (getCurrentTime)
import           Database                        (insertToken_,
                                                  makeTablesIfNotExists)
import           Database.Beam.Sqlite.Connection (runBeamSqliteDebug)
import           Database.SQLite.Simple          (close, open)
import           Database.Table.Token            (TokenType (..))
import           Network.Wai.Handler.Warp        (run)
import           Path.IO                         (resolveFile')
import           YamlParse.Applicative           (YamlParser, explainParser,
                                                  prettySchema, readConfigFile,
                                                  yamlSchema)

basicInsert :: IO ()
basicInsert = do
  conn <- open "test.db"
  makeTablesIfNotExists conn
  utcTime <- getCurrentTime
  r <-
    runBeamSqliteDebug putStrLn conn $
    insertToken_ UserToken "notRealBearer" utcTime
  print r
  close conn

main :: IO ()
main = do
  configPath <- resolveFile' "./config.yaml"
  config' <- readConfigFile configPath :: IO (Maybe Config)
  -- TODO
  -- - If Nothing, we should stop...
  case config' of
    Just config
      -- TODO: Should have connection pool to feed to app
     -> do
      conn <- open "test.db"
      makeTablesIfNotExists conn
      close conn
      run 8081 (app config)
    Nothing -> pure ()
