module Main exposing (main)

-- import Html.Styled.Events exposing (onClick)
-- import Svg.Styled as StyledSvg
-- import Svg.Styled.Attributes as SvgAttribs

import Browser
import Browser.Dom as Dom
import Canvas exposing (..)
import Canvas.Settings as Canvas exposing (..)
import Canvas.Settings.Advanced as Canvas exposing (..)
import Canvas.Settings.Text as Canvas exposing (..)
import Canvas.Texture as Texture
import Color
import Css exposing (..)
import Html.Styled exposing (..)
import Task


drawTextAt : Point -> String -> Renderable
drawTextAt pos txt =
    Canvas.text
        [ font { size = 48, family = "sans-serif" }, Canvas.fill Color.purple ]
        pos
        txt


drawScaledTexture : Texture.Texture -> Float -> Point -> Renderable
drawScaledTexture texture scale ( x, y ) =
    Canvas.texture
        [ Canvas.transform [ Canvas.scale scale scale ] ]
        ( x / scale, y / scale )
        texture


view : Model -> Html Msg
view model =
    Canvas.toHtmlWith
        { width = 1920
        , height = 1080
        , textures =
            [ Texture.loadFromImageUrl "./snail.png"
                (\mTexture ->
                    case mTexture of
                        Just texture ->
                            SnailTextureLoaded texture

                        Nothing ->
                            NoOp
                )
            ]
        }
        []
        ([ shapes [ Canvas.fill Color.white ] [ rect ( 0, 0 ) 50 50 ]
         , shapes [ Canvas.fill Color.red ] [ rect ( 50, 50 ) 100 100 ]
         , drawTextAt ( 300, 300 ) "Hello, world..."
         ]
            ++ (case model.snailTexture of
                    Just texture ->
                        [ drawScaledTexture texture 0.05 ( 400, 400 )
                        ]

                    Nothing ->
                        []
               )
        )
        |> fromUnstyled



-- view : Model -> Html Msg
-- view _ =
--     div
--         [ css
--             [ width (px 1920)
--             , height (px 1080)
--             ]
--         ]
--         [ div
--             [ css
--                 [ Tw.text_7xl
--                 , Tw.text_purple_900
--                 , position absolute
--                 , top (px 1800)
--                 , left (px 800)
--                 ]
--             ]
--             [ text "Followers 12" ]
--         , div
--             [ css
--                 [ Tw.text_7xl
--                 , Tw.text_purple_900
--                 , position absolute
--                 , top (px 1400)
--                 , left (px 400)
--                 ]
--             ]
--             [ text "Subscriber 1234" ]
--         , div
--             [ css
--                 [ position absolute
--                 , top (px 1000)
--                 , left (px 200)
--                 , transform (scale 0.5)
--                 ]
--             ]
--             [ snail ]
--         ]


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

        SnailTextureLoaded texture ->
            ( { model | snailTexture = Just texture }, Cmd.none )


type Msg
    = NoOp
    | SnailTextureLoaded Texture.Texture


setViewport : Cmd Msg
setViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 1920 1080)


type alias Model =
    { snailTexture : Maybe Texture.Texture
    }



-- First argument is flags


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { snailTexture = Nothing }, setViewport )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
