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
getServerSentEventsR = pollingEventSource () serverSentEventsGenerator

fromText :: Text -> Builder
fromText = fromByteString . TE.encodeUtf8

fromJSONToBuilder :: ToJSON a => a -> Builder
fromJSONToBuilder = fromLazyByteString . encode

serverSentEventsGenerator :: EventSourcePolyfill -> s -> Handler ([ServerEvent], s)
serverSentEventsGenerator _ s = do
  liftIO $ threadDelay 1_000_000
  events <- runDB $ select $ from queryMostRecentIncompleteEvent
  case events of
    [Entity queueId Queue {..}] -> do
      if queueCompleted
        then
            pure ( [ServerEvent
                  (Just "message")
                  (Just . fromText $ tshow queueId)
                  [fromJSONToBuilder PingMessage]
                ]
              , s
              )
        else
          do
            -- Mark the event as completed, we should have a replay system at some point
            void . runDB . update $ \q -> do
              set q [QueueCompleted =. val True]
              where_ $ q ^. #id ==. val queueId
            case queueEventKind of
              Ping -> do
                liftIO $ putStrLn "Sent ping event from database"
                pure ( [ServerEvent
                      (Just "message")
                      (Just . fromText $ tshow queueId)
                      [fromJSONToBuilder PingMessage]
                    ]
                  , s
                  )
              NewFollower -> do
                mFollowerEvent <- runDB $ get (coerce queueId :: FollowerEventId)
                liftIO $ putStrLn "Sent follower event from database"
                pure $ case mFollowerEvent of
                  Nothing -> ([], s)
                  Just FollowerEvent {..} ->
                    ( [ServerEvent
                          (Just "message")
                          (Just . fromText $ tshow queueId)
                          [fromJSONToBuilder $ FollowMessage followerEventTwitchUserName]
                        ]
                      , s
                    )
              NewSubscriber -> do
                mSubscriberEvent <- runDB $ get (coerce queueId :: SubscriberEventId)
                liftIO $ putStrLn "Sent subscriber event from database"
                pure $ case mSubscriberEvent of
                  Nothing -> ([], s)
                  Just SubscriberEvent {..} ->
                    ( [ServerEvent
                        (Just "message")
                        (Just . fromText $ tshow queueId)
                        [fromJSONToBuilder $ SubscribeMessage subscriberEventTwitchUserName]
                      ]
                    , s
                    )
              NewCheer -> do
                mCheerEvent <- runDB $ get (coerce queueId :: CheerEventId)
                liftIO $ putStrLn "Sent cheer event from database"
                pure $ case mCheerEvent of
                  Nothing -> ([], s)
                  Just CheerEvent {..} ->
                    ( [ServerEvent
                        (Just "message")
                        (Just . fromText $ tshow queueId)
                        [fromJSONToBuilder $ CheerMessage cheerEventTwitchUserName cheerEventBits]
                      ]
                    , s
                    )
              NewRaid -> do
                mRaidEvent <- runDB $ get (coerce queueId :: RaidEventId)
                liftIO $ putStrLn "Sent raid event from database"
                pure $ case mRaidEvent of
                  Nothing -> ([], s)
                  Just RaidEvent {..} ->
                    ( [ServerEvent
                        (Just "message")
                        (Just . fromText $ tshow queueId)
                        [fromJSONToBuilder $ RaidMessage raidEventTwitchUserName raidEventViewers]
                      ]
                    , s
                    )
    -- If there are no events, send 0 ping
    _ ->
      pure ( [ServerEvent
            (Just "message")
            (Just . fromText $ tshow (0 :: Int))
            [fromJSONToBuilder PingMessage]
          ]
        , s
        )

queryMostRecentIncompleteEvent :: SqlQuery (SqlExpr (Entity Queue))
queryMostRecentIncompleteEvent = do
  queue <- from $ table @Queue
  orderBy [asc (queue ^. #id)]
  limit 1
  pure queue
