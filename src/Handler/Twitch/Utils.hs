module Handler.Twitch.Utils where

import Data.Twitch
import Database.Esqueleto.Experimental qualified as Db
import Import
import Request.Twitch.Sql              (queryCredentials)

-- | Ensure the session contains a valid 'TwitchUser' and then
-- grab their credentials
getTwitchCredentials :: Handler (Either TwitchError TwitchCredentials)
getTwitchCredentials = do
  mTwitchUser <- maybeAuthId
  case mTwitchUser of
    Nothing -> sendStatusJSON @_ @Text status401 "Please log in"
    Just twitchUserId -> do
      mTwitchCredentials <- runDB . Db.selectOne $ queryCredentials twitchUserId
      pure $ case mTwitchCredentials of
        Nothing               -> Left UnableToFetchCredentials
        Just (Entity _ creds) -> Right creds
