{-# LANGUAGE NumericUnderscores #-}

module Handler.ServerSentEvents where

import Import
import Yesod.EventSource
import Network.Wai.EventSource.EventStream
import Control.Concurrent

getServerSentEventsR :: Handler TypedContent
getServerSentEventsR = pollingEventSource () serverSentEventsGenerator

serverSentEventsGenerator :: EventSourcePolyfill -> s -> HandlerFor site ([ServerEvent], s)
serverSentEventsGenerator _ s = do
  liftIO $ threadDelay 1_000_000
  pure ([ServerEvent (Just "message") (Just "1") ["hello"]], s)
