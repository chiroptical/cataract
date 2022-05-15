port module Main exposing (main)

-- import Svg.Styled as StyledSvg
-- import Svg.Styled.Attributes as SvgAttribs

import Animator
import Animator.Inline
import Browser
import Browser.Dom as Dom
import Css exposing (..)
import Data.ServerSentEvents as SSE
import Debug as Debug
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Json.Decode as Decode
import Platform.Sub as Sub
import Tailwind.Utilities as Tw
import Task
import Time



-- PORTS


port sseOpenReceiver : (String -> msg) -> Sub msg


port sseErrorReceiver : (String -> msg) -> Sub msg


port sseMessageReceiver : (String -> msg) -> Sub msg


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
                , top (px 200)
                , left (px 200)
                ]
            , Attributes.fromUnstyled <|
                Animator.Inline.opacity model.textAnimate <|
                    \( state, _ ) ->
                        if state then
                            Animator.at 1

                        else
                            Animator.at 0
            ]
            [ text <| Tuple.second <| Animator.current model.textAnimate
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


uncons : List a -> Maybe ( a, List a )
uncons xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            Just ( y, ys )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            let
                -- If the animation queue is empty, we are no longer awaiting
                -- animations. If it is not, queue the next animation
                endOrAnimate : ( Model, Cmd Msg )
                endOrAnimate =
                    if List.isEmpty model.textAnimationQueue then
                        ( { model | textAnimationWaitingFor = Nothing }
                            |> Animator.update newTime animator
                        , Cmd.none
                        )

                    else
                        ( model
                            |> Animator.update newTime animator
                        , Task.perform AnimateText (Task.succeed ())
                        )
            in
            case model.textAnimationWaitingFor of
                -- If we aren't waiting for any animations, check to see if we need
                -- to queue a new one
                Nothing ->
                    endOrAnimate

                Just state ->
                    if Animator.arrivedAt state newTime model.textAnimate then
                        -- We have arrived at the end of the animation loop. Finalize
                        -- or queue up the next animation
                        endOrAnimate

                    else
                        -- We are still waiting for the current animation to finish,
                        -- just tick
                        ( model
                            |> Animator.update newTime animator
                        , Cmd.none
                        )

        AnimateText _ ->
            case uncons model.textAnimationQueue of
                Nothing ->
                    ( model, Cmd.none )

                Just ( text, tail ) ->
                    ( { model
                        | textAnimate =
                            model.textAnimate
                                |> Animator.queue
                                    [ Animator.event (Animator.millis 2000) ( True, text )
                                    , Animator.event (Animator.millis 2000) ( False, text )
                                    ]
                        , textAnimationQueue = tail
                        , textAnimationWaitingFor = Just ( False, text )
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
                    let
                        humanReadable =
                            SSE.serverSentEventDataToString sseData
                    in
                    Debug.log humanReadable
                        ( case sseData of
                            SSE.PingMessageData _ ->
                                model

                            _ ->
                                { model
                                    | textAnimationQueue =
                                        model.textAnimationQueue ++ [ humanReadable ]
                                }
                        , Cmd.none
                        )

                Err err ->
                    Debug.log ("withInput:" ++ input ++ ", gotError: " ++ Decode.errorToString err)
                        ( model, Cmd.none )


type Msg
    = NoOp
    | Tick Time.Posix
    | AnimateText ()
    | ServerSentEventOpen
    | ServerSentEventError String
    | ServerSentEventMessage String (Result Decode.Error SSE.ServerSentEventData)


setViewport : Cmd Msg
setViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 1920 1080)


type alias Model =
    { textAnimate : Animator.Timeline ( Bool, String )
    , textAnimationQueue : List String
    , textAnimationWaitingFor : Maybe ( Bool, String )
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
    ( { textAnimate = Animator.init ( False, "" )
      , textAnimationQueue = []
      , textAnimationWaitingFor = Nothing
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
        , sseMessageReceiver (\str -> ServerSentEventMessage str (Decode.decodeString SSE.serverSentEventDataDecoder str))
        ]
