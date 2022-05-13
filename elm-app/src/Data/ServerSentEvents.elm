module Data.ServerSentEvents exposing
    ( ServerSentEventData(..)
    , serverSentEventDataDecoder
    , serverSentEventDataToString
    )

import Json.Decode as Decode
import String as String


type alias Kind =
    { kind : String
    }


kindDecoder : String -> Decode.Decoder Kind
kindDecoder match =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\str ->
                if match == str then
                    Decode.succeed (Kind str)

                else
                    Decode.fail ("invalid kind, got: " ++ str ++ " wanted: " ++ match)
            )


type ServerSentEventData
    = PingMessageData PingMessage
    | FollowMessageData FollowMessage
    | SubscriberMessageData SubscriberMessage
    | CheerMessageData CheerMessage
    | RaidMessageData RaidMessage


serverSentEventDataDecoder : Decode.Decoder ServerSentEventData
serverSentEventDataDecoder =
    Decode.oneOf
        [ Decode.map PingMessageData pingMessageDecoder
        , Decode.map FollowMessageData followMessageDecoder
        , Decode.map SubscriberMessageData subscriberMessageDecoder
        , Decode.map CheerMessageData cheerMessageDecoder
        , Decode.map RaidMessageData raidMessageDecoder
        ]


serverSentEventDataToString : ServerSentEventData -> String
serverSentEventDataToString data =
    case data of
        PingMessageData _ ->
            "pong"

        FollowMessageData { twitchUserName } ->
            twitchUserName ++ " followed the stream"

        SubscriberMessageData { twitchUserName } ->
            twitchUserName ++ " subscribed to the stream"

        CheerMessageData { twitchUserName, numberOfBits } ->
            twitchUserName ++ " cheered " ++ String.fromInt numberOfBits ++ " bits"

        RaidMessageData { twitchUserName, numberOfRaiders } ->
            twitchUserName ++ " raided with " ++ String.fromInt numberOfRaiders ++ " viewers"


type alias PingMessage =
    {}


pingMessageDecoder : Decode.Decoder PingMessage
pingMessageDecoder =
    Decode.map (\_ -> PingMessage) (kindDecoder "ping")


type alias FollowMessage =
    { twitchUserName : String
    }


followMessageDecoder : Decode.Decoder FollowMessage
followMessageDecoder =
    Decode.map2
        (\_ twitchUserName -> FollowMessage twitchUserName)
        (kindDecoder "follow")
        (Decode.field "from" Decode.string)


type alias SubscriberMessage =
    { twitchUserName : String
    }


subscriberMessageDecoder : Decode.Decoder SubscriberMessage
subscriberMessageDecoder =
    Decode.map2
        (\_ twitchUserName -> SubscriberMessage twitchUserName)
        (kindDecoder "subscription")
        (Decode.field "from" Decode.string)


type alias CheerMessage =
    { twitchUserName : String
    , numberOfBits : Int
    }


cheerMessageDecoder : Decode.Decoder CheerMessage
cheerMessageDecoder =
    Decode.map3
        (\_ twitchUserName numberOfBits -> CheerMessage twitchUserName numberOfBits)
        (kindDecoder "cheer")
        (Decode.field "from" Decode.string)
        (Decode.field "bits" Decode.int)


type alias RaidMessage =
    { twitchUserName : String
    , numberOfRaiders : Int
    }


raidMessageDecoder : Decode.Decoder RaidMessage
raidMessageDecoder =
    Decode.map3
        (\_ twitchUserName numberOfBits -> RaidMessage twitchUserName numberOfBits)
        (kindDecoder "raid")
        (Decode.field "from" Decode.string)
        (Decode.field "raiderCount" Decode.int)
