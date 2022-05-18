module Page.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Platform.Sub as Sub
import Route
import Session exposing (Session)
import Tailwind.Utilities as Tw


view : Model -> Html Msg
view _ =
    -- <nav class="bg-white shadow">
    nav
        [ css
            [ Tw.bg_white
            , Tw.shadow
            ]
        ]
        -- <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        [ div
            [ css
                [ Tw.max_w_7xl
                , Tw.mx_auto
                , Tw.px_4
                , Tw.px_6
                , Tw.px_8
                ]
            ]
            -- <div class="flex justify-between h-16">
            [ div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.h_16
                    ]
                ]
                -- <div class="flex">
                [ div
                    [ css [ Tw.flex ] ]
                    [ div
                        [ css
                            [ Tw.hidden
                            , Tw.ml_6
                            , Tw.flex
                            , Tw.space_x_8
                            ]
                        ]
                        [ a
                            [ css
                                [ Tw.border_indigo_500
                                , Tw.text_gray_900
                                , Tw.inline_flex
                                , Tw.items_center
                                , Tw.px_1
                                , Tw.pt_1
                                , Tw.border_b_2
                                , Tw.text_sm
                                , Tw.font_medium
                                ]
                            , Route.href Route.Home
                            ]
                            [ text "Home" ]
                        , a
                            [ css
                                [ Tw.border_indigo_500
                                , Tw.text_gray_900
                                , Tw.inline_flex
                                , Tw.items_center
                                , Tw.px_1
                                , Tw.pt_1
                                , Tw.border_b_2
                                , Tw.text_sm
                                , Tw.font_medium
                                ]
                            , Route.href Route.Overlay
                            ]
                            [ text "Overlay" ]
                        ]
                    ]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


type Msg
    = NoOp


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
