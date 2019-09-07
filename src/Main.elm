module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random exposing (Seed)
import Time
import Uuid exposing (Uuid)



---- MODEL ----


type alias Model =
    { list : List String
    , id : ( Maybe Uuid, Seed )
    }


init : Int -> ( Model, Cmd Msg )
init seed =
    ( { list = []
      , id = ( Nothing, Random.initialSeed seed )
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AddUuid
    | Tick Time.Posix
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            ( { model | list = [] }, Cmd.none )

        AddUuid ->
            addId model

        Tick _ ->
            if List.length model.list < 3 then
                addId model

            else
                ( model, Cmd.none )


addId : Model -> ( Model, Cmd Msg )
addId model =
    let
        ( _, oldSeed ) =
            model.id

        ( newId, newSeed ) =
            Random.step Uuid.uuidGenerator oldSeed
    in
    ( { model
        | id = ( Just newId, newSeed )
        , list = Uuid.toString newId :: model.list
      }
    , Cmd.none
    )



---- VIEW ----


showModel : Model -> Html Msg
showModel model =
    p [] [ div [] (List.map (\v -> div [] [ text v ]) model.list) ]


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working! Yes." ]
        , button [ onClick AddUuid ] [ text "add uuid" ]
        , button [ onClick Clear ] [ text "clear" ]
        , showModel model
        ]



---- PROGRAM ----
-- main : Program () Model Msg


main : Program Int Model Msg
main =
    Browser.element
        { view = view

        -- , init = \_ -> init
        , init = init
        , update = update
        , subscriptions = subscriptions

        --, subscriptions = always Sub.none
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 5000 Tick
