module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    List String


init : ( Model, Cmd Msg )
init =
    ( [], Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model ++ [ "1" ], Cmd.none )



---- VIEW ----


showModel : Model -> List (Html Msg)
showModel model =
    List.map (\v -> h1 [] [ text v ]) model


view : Model -> Html Msg
view model =
    div []
        ([ img [ src "/logo.svg" ] []
         , h1 [] [ text "Your Elm App is working! Yes." ]
         , button [ onClick NoOp ] [ text "add uuid" ]
         ]
            ++ showModel model
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
