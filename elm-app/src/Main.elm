module Main exposing (main)

-- import Svg.Styled as StyledSvg
-- import Svg.Styled.Attributes as SvgAttribs

import Animator
import Animator.Inline
import Browser
import Browser.Dom as Dom
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Tailwind.Utilities as Tw
import Task
import Time


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


type Msg
    = NoOp
    | Tick Time.Posix
    | AnimateText


setViewport : Cmd Msg
setViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 1920 1080)


type alias Model =
    { textAnimate : Animator.Timeline Bool
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
      }
    , setViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    animator
        |> Animator.toSubscription Tick model
