{-# LANGUAGE TypeApplications #-}
module Handler.Twitch.Followers where

import Import
import Request.Twitch
import Request.Twitch.Followers
import Database.Esqueleto.Experimental qualified as Db
import Data.Text qualified as Text
import Request.Twitch.Sql (queryCredentials)

getFollowersR :: Handler Text
getFollowersR = do
  TwitchSettings {..} <- appTwitchSettings <$> getsYesod appSettings
  mTwitchUser <- maybeAuthId
  case mTwitchUser of
    Nothing -> permissionDenied "mTwitchUser is Nothing"
    Just twitchUserId -> do
      mTwitchCredentials <- runDB $ Db.selectOne $ queryCredentials twitchUserId
      case mTwitchCredentials of
        Nothing -> permissionDenied "mTwitchCredentials is Nothing"
        Just (Entity _ creds) -> do
          eResponse <- twitchRequest @Followers twitchSettingsClientId creds FollowersPayload
          case eResponse of
            Left _ -> permissionDenied "eResponse is Left"
            Right FollowersResponse {..} -> pure . Text.pack $ show followersResponseTotal 
