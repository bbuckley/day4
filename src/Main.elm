module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, li, ol, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random exposing (Seed)
import Uuid exposing (Uuid)



---- MODEL ----


type alias Uid =
    String


type alias Model =
    { list : List Uid
    , id : ( Maybe Uuid, Seed )
    }


init : ( Model, Cmd Msg )
init =
    ( { list = []
      , id = ( Nothing, Random.initialSeed 0 )
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AddUuid


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddUuid ->
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
