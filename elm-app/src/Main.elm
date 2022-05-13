port module Main exposing (main)

-- import Svg.Styled as StyledSvg
-- import Svg.Styled.Attributes as SvgAttribs

import Animator
import Animator.Inline
import Browser
import Browser.Dom as Dom
import Css exposing (..)
import Debug as Debug
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Platform.Sub as Sub
import Tailwind.Utilities as Tw
import Task
import Time



-- PORTS


port sseOpenReceiver : (String -> msg) -> Sub msg


port sseErrorReceiver : (String -> msg) -> Sub msg


port sseMessageReceiver : (String -> msg) -> Sub msg



-- JSON


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



-- TODO: Sum type that stores that associates a kind to its' data


type ServerSentEventData
    = PingMessageData PingMessage


serverSentEventDataDecoder : Decode.Decoder ServerSentEventData
serverSentEventDataDecoder =
    Decode.oneOf
        [ Decode.map PingMessageData pingMessageDecoder
        ]


serverSentEventDataToString : ServerSentEventData -> String
serverSentEventDataToString data =
    case data of
        PingMessageData _ ->
            "pong"


type alias PingMessage =
    {}


pingMessageDecoder : Decode.Decoder PingMessage
pingMessageDecoder =
    Decode.map (\_ -> PingMessage) (kindDecoder "ping")


view : Model -> Html Msg
view model =
    div
        [ css
            [ width (px 1920)
            , height (px 1080)
            ]
        ]
        [ div
            [ css
                [ Tw.text_7xl
                , Tw.text_purple_900
                , position absolute
                , top (px 100)
                , left (px 100)
                ]
            ]
            [ text "Followers 12" ]
        , div
            [ css
                [ Tw.text_7xl
                , Tw.text_purple_900
                , position absolute
                , top (px 300)
                , left (px 300)
                ]
            ]
            [ text model.decodeResult ]
        , div
            [ css
                [ Tw.text_7xl
                , Tw.text_purple_900
                , position absolute
                , top (px 200)
                , left (px 200)
                ]
            , Attributes.fromUnstyled <|
                Animator.Inline.opacity model.textAnimate <|
                    \state ->
                        if state then
                            Animator.at 1

                        else
                            Animator.at 0
            ]
            [ text "Subscriber 1234"
            ]
        , div
            [ onClick AnimateText
            ]
            [ button
                [ css
                    [ Tw.rounded_full
                    ]
                ]
                [ text "animate" ]
            ]

        -- , div
        --     [ css
        --         [ position absolute
        --         , top (px 1000)
        --         , left (px 200)
        --         , transform (scale 0.5)
        --         ]
        --     ]
        --     [ snail ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , update = update
        , init = initialModel
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        AnimateText ->
            ( { model
                | textAnimate =
                    model.textAnimate
                        |> Animator.queue
                            [ Animator.event (Animator.millis 2000) True
                            , Animator.event (Animator.millis 2000) False
                            ]
              }
            , Cmd.none
            )

        ServerSentEventOpen ->
            Debug.log "ServerSentEventOpen"
                ( model
                , Cmd.none
                )

        ServerSentEventError err ->
            Debug.log err
                ( model
                , Cmd.none
                )

        ServerSentEventMessage input result ->
            case result of
                Ok sseData ->
                    Debug.log (serverSentEventDataToString sseData)
                        ( { model | decodeResult = "It worked!" }, Cmd.none )

                Err err ->
                    Debug.log ("withInput:" ++ input ++ ", gotError: " ++ Decode.errorToString err)
                        ( { model | decodeResult = "It didn't work!" }, Cmd.none )


type Msg
    = NoOp
    | Tick Time.Posix
    | AnimateText
    | ServerSentEventOpen
    | ServerSentEventError String
    | ServerSentEventMessage String (Result Decode.Error ServerSentEventData)


setViewport : Cmd Msg
setViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 1920 1080)


type alias Model =
    { textAnimate : Animator.Timeline Bool
    , decodeResult : String
    }


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .textAnimate
            (\new model ->
                { model | textAnimate = new }
            )


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { textAnimate = Animator.init False
      , decodeResult = ""
      }
    , setViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ animator
            |> Animator.toSubscription Tick model
        , sseOpenReceiver (\_ -> ServerSentEventOpen)
        , sseErrorReceiver ServerSentEventError
        , sseMessageReceiver (\str -> ServerSentEventMessage str (Decode.decodeString serverSentEventDataDecoder str))
        ]
