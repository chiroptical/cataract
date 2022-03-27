{-# LANGUAGE TypeApplications #-}

module Request.Twitch.Sql where

import Database.Esqueleto.Experimental
import Import.NoFoundation hiding ((==.))

queryCredentials :: TwitchUserId -> SqlQuery (SqlExpr (Entity TwitchCredentials))
queryCredentials twitchUserId = do
  tc <- from $ table @TwitchCredentials
  where_ $ tc ^. TwitchCredentialsTwitchUserId ==. val twitchUserId
  pure tc
