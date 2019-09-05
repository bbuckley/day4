module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, p, text)
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
    = AddUuid


newUuid : Model -> String
newUuid model =
    "uuid" ++ String.fromInt (List.length model + 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddUuid ->
            ( newUuid model :: model, Cmd.none )



---- VIEW ----


showModel : Model -> Html Msg
showModel model =
    p [] [ div [] (List.map (\v -> p [] [ text v ]) model) ]


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working! Yes." ]
        , button [ onClick AddUuid ] [ text "add uuid" ]
        , showModel model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
