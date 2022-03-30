{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}

module Request.Twitch.Sql where

import Database.Esqueleto.Experimental
import Import.NoFoundation hiding ((==.), on)

queryCredentials :: TwitchUserId -> SqlQuery (SqlExpr (Entity TwitchCredentials))
queryCredentials twitchUserId = do
  tc <- from $ table @TwitchCredentials
  where_ $ tc ^. #twitchUserId ==. val twitchUserId
  pure tc

queryCredentialsFromIdent :: Text -> SqlQuery (SqlExpr (Entity TwitchCredentials))
queryCredentialsFromIdent twitchUserIdent = do
  (tu :& tc) <-
    from $
      table @TwitchUser
      `innerJoin` table @TwitchCredentials
      `on` (\(tu :& tc) ->
              tc ^. #twitchUserId ==. tu ^. #id
           )
  where_ $ tu ^. #ident ==. val twitchUserIdent
  pure tc
