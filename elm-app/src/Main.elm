port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Data.ServerSentEvents as SSE
import Debug as Debug
import Html.Styled exposing (..)
import Json.Decode as Decode
import Page.Home as Home
import Page.Overlay as Overlay
import Platform.Sub as Sub
import Route as Route exposing (Route)
import Session exposing (Session)
import Url



-- PORTS


port sseOpenReceiver : (String -> msg) -> Sub msg


port sseErrorReceiver : (String -> msg) -> Sub msg


port sseMessageReceiver : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = toBrowserDocument
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Overlay Overlay.Model


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home model ->
            model.session

        Overlay model ->
            model.session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        log =
            case Route.fromUrl url of
                Nothing ->
                    "fromUrl -> Nothing"

                Just r ->
                    Route.toString r
    in
    Debug.log
        log
        (changeRouteTo
            (Route.fromUrl url)
            (Redirect (Session key))
        )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl session.navKey Route.Home )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Overlay ->
            Overlay.initialModel session
                |> updateWith Overlay GotOverlayMsg model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotOverlayMsg Overlay.Msg
    | GotHomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model, Nav.pushUrl (toSession model).navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotOverlayMsg subMsg, Overlay subModel ) ->
            Overlay.update subMsg subModel
                |> updateWith Overlay GotOverlayMsg model

        _ ->
            ( model, Cmd.none )


updateWith :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ sseOpenReceiver (\_ -> GotOverlayMsg Overlay.ServerSentEventOpen)
        , sseErrorReceiver (GotOverlayMsg << Overlay.ServerSentEventError)
        , sseMessageReceiver
            (\str ->
                GotOverlayMsg
                    (Overlay.ServerSentEventMessage
                        str
                        (Decode.decodeString SSE.serverSentEventDataDecoder str)
                    )
            )
        ]


toBrowserDocument : Model -> Browser.Document Msg
toBrowserDocument model =
    case model of
        Overlay _ ->
            { title = "Overlay"
            , body = [ view model |> toUnstyled ]
            }

        _ ->
            { title = "Placeholder"
            , body = [ view model |> toUnstyled ]
            }


view : Model -> Html Msg
view model =
    case model of
        Overlay overlayModel ->
            Html.Styled.map GotOverlayMsg (Overlay.view overlayModel)

        Home homeModel ->
            Html.Styled.map GotHomeMsg (Home.view homeModel)

        _ ->
            div [] [ text "Followers 12" ]
