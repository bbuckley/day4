module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, li, ol, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random exposing (Seed, initialSeed, step)
import Uuid exposing (Uuid)



---- MODEL ----


type alias Uid =
    String


type alias Model =
    { list : List Uid
    , currentSeed : Seed
    , currentUuid : Maybe Uuid
    }


init : ( Model, Cmd Msg )
init =
    ( { list = []
      , currentSeed = initialSeed 0
      , currentUuid = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AddUuid


newUuid : Model -> Uid
newUuid model =
    -- "!uuid" ++ String.fromInt (List.length model.list + 1)
    case model.currentUuid of
        Nothing ->
            "first"

        Just id ->
            Uuid.toString id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddUuid ->
            let
                ( newId, newSeed ) =
                    step Uuid.uuidGenerator model.currentSeed
            in
            ( { model
                | currentUuid = Just newId
                , currentSeed = newSeed
                , list = newUuid model :: model.list
              }
            , Cmd.none
            )



---- VIEW ----


showModel : Model -> Html Msg
showModel model =
    p [] [ ol [] (List.map (\v -> li [] [ text v ]) model.list) ]


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
