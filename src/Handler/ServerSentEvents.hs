{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels   #-}

module Handler.ServerSentEvents where

import Control.Concurrent
import Data.Aeson                          (encode)
import Data.Binary.Builder
import Data.Coerce                         (coerce)
import Data.Event                          (Event (..))
import Data.Message
import Data.Text.Encoding                  qualified as TE
import Database.Esqueleto.Experimental
import Import                              hiding (Builder, on, update, (=.),
                                            (==.))
import Network.Wai.EventSource.EventStream
import Yesod.EventSource

getServerSentEventsR :: Handler TypedContent
getServerSentEventsR = pollingEventSource (0 :: Integer) serverSentEventsGenerator

fromText :: Text -> Builder
fromText = fromByteString . TE.encodeUtf8

fromJSONToBuilder :: ToJSON a => a -> Builder
fromJSONToBuilder = fromLazyByteString . encode

mkServerEventList :: ToJSON a => Integer -> a -> [ServerEvent]
mkServerEventList nextNumber obj =
  [ ServerEvent
      (Just "message")
      (Just . fromText $ tshow nextNumber)
      [fromJSONToBuilder obj]
  ]

serverSentEventsGenerator :: EventSourcePolyfill -> Integer -> Handler ([ServerEvent], Integer)
serverSentEventsGenerator _ currentNumber = do
  liftIO $ threadDelay 1_000_000
  let nextNumber = currentNumber + 1
      mkServerEventMsg msg = (mkServerEventList currentNumber msg, nextNumber)
  mLeastRecentIncomplete <- runDB . select $ from queryLeastRecentIncompleteEvent
  case mLeastRecentIncomplete of
    [Entity queueId Queue {..}] -> do
        -- Mark the event as completed, we should have a replay system at some point
        void . runDB . update $ \q -> do
          set q [QueueCompleted =. val True]
          where_ $ q ^. #id ==. val queueId
        case queueEventKind of
          Ping -> pure $ mkServerEventMsg PingMessage
          NewFollower -> do
            mFollowerEvent <- runDB $ get (coerce queueId :: FollowerEventId)
            pure $ case mFollowerEvent of
              Nothing -> mkServerEventMsg NoMatchingEventMessage
              Just FollowerEvent {..} -> mkServerEventMsg $ FollowMessage followerEventTwitchUserName
          NewSubscriber -> do
            mSubscriberEvent <- runDB $ get (coerce queueId :: SubscriberEventId)
            pure $ case mSubscriberEvent of
              Nothing -> mkServerEventMsg NoMatchingEventMessage
              Just SubscriberEvent {..} -> mkServerEventMsg $ SubscribeMessage subscriberEventTwitchUserName
          NewCheer -> do
            mCheerEvent <- runDB $ get (coerce queueId :: CheerEventId)
            pure $ case mCheerEvent of
              Nothing -> mkServerEventMsg NoMatchingEventMessage
              Just CheerEvent {..} -> mkServerEventMsg $ CheerMessage cheerEventTwitchUserName cheerEventBits
          NewRaid -> do
            mRaidEvent <- runDB $ get (coerce queueId :: RaidEventId)
            pure $ case mRaidEvent of
              Nothing -> mkServerEventMsg NoMatchingEventMessage
              Just RaidEvent {..} -> mkServerEventMsg $ RaidMessage raidEventTwitchUserName raidEventViewers
    -- The default action to keep the connection open, do not include an event id
    _ -> pure $ mkServerEventMsg PingMessage

queryLeastRecentIncompleteEvent :: SqlQuery (SqlExpr (Entity Queue))
queryLeastRecentIncompleteEvent = do
  queue <- from $ table @Queue
  where_ $ queue ^. #completed ==. val False
  orderBy [asc (queue ^. #id)]
  limit 1
  pure queue
