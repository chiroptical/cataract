module Route exposing (Route(..), fromUrl, href, parser, replaceUrl, toString)

import Browser.Navigation as Nav
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)


type Route
    = Root
    | Home
    | Overlay


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Overlay (s "overlay")
        ]


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toString route)


href : Route -> Attribute msg
href targetRoute =
    Attributes.href (toString targetRoute)


toString : Route -> String
toString page =
    "#/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Root ->
            []

        Home ->
            []

        Overlay ->
            [ "overlay" ]


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
