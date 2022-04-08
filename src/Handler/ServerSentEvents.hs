{-# LANGUAGE NumericUnderscores #-}

module Handler.ServerSentEvents where

import Control.Concurrent
import Import
import Network.Wai.EventSource.EventStream
import Yesod.EventSource

getServerSentEventsR :: Handler TypedContent
getServerSentEventsR = pollingEventSource () serverSentEventsGenerator

serverSentEventsGenerator :: EventSourcePolyfill -> s -> HandlerFor site ([ServerEvent], s)
serverSentEventsGenerator _ s = do
  liftIO $ threadDelay 1_000_000
  pure ([ServerEvent (Just "message") (Just "1") ["hello"]], s)
