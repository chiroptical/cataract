module Route exposing (..)

import Html as H exposing (Html)
import Page.Home as Home
import Page.Overlay as Overlay
import Router
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Home
    | Overlay


type Msg
    = NoMsg


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Router.mapRoute Parser.top Home
        , Router.mapRoute (Parser.s "overlay") Overlay
        ]


view : Route -> List (Html Msg)
view route =
    case route of
        Home ->
            Home.view

        Overlay ->
            Overlay.view


update : Msg -> ( Route, Cmd Msg )
update _ =
    ( Home, Cmd.none )


subscriptions : Route -> Sub Msg
subscriptions route =
    case route of
        Home ->
            Sub.none

        Overlay ->
            Sub.none


title : Route -> Maybe String
title route =
    case route of
        Home ->
            Nothing

        Overlay ->
            Just "Overlay"


notFound : Url -> List (Html msg)
notFound url =
    [ H.h1 [] [ H.text "Page not found!" ]
    , H.h3 [] [ H.text <| Url.toString url ]
    ]
