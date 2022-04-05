module Main exposing (main)

-- import Html.Styled.Events exposing (onClick)
-- import Svg.Styled as StyledSvg
-- import Svg.Styled.Attributes as SvgAttribs

import Browser
import Browser.Dom as Dom
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Svg.Snail exposing (snail)
import Tailwind.Utilities as Tw
import Task


view : Model -> Html Msg
view _ =
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
                , top (px 1800)
                , left (px 800)
                ]
            ]
            [ text "Followers 12" ]
        , div
            [ css
                [ Tw.text_7xl
                , Tw.text_purple_900
                , position absolute
                , top (px 1400)
                , left (px 400)
                ]
            ]
            [ text "Subscriber 1234" ]
        , div
            [ css
                [ position absolute
                , top (px 1000)
                , left (px 200)
                , transform (scale 0.5)
                ]
            ]
            [ snail ]
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
update _ model =
    ( model, Cmd.none )


type Msg
    = NoOp


setViewport : Cmd Msg
setViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 1920 1080)


type alias Model =
    ()



-- First argument is flags


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( (), setViewport )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
