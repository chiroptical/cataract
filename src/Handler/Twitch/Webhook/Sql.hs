{-# LANGUAGE OverloadedLabels #-}

module Handler.Twitch.Webhook.Sql where

import Database.Esqueleto.Experimental
import Import.NoFoundation hiding (on, (==.))

getRecentNEventsOffQueue :: Int64 -> SqlQuery (SqlExpr (Entity Queue), SqlExpr (Entity Event))
getRecentNEventsOffQueue lim = do
    (queue :& event) <-
        from $
            table @Queue
                `innerJoin` table @Event
                `on` ( \(q :& e) ->
                        q ^. #eventId ==. e ^. #id
                     )
    orderBy [desc (queue ^. #id)]
    limit lim
    pure (queue, event)
