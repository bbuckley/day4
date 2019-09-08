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
    { id : ( Maybe Uuid, Seed )
    , list : List String
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
    | AddUuids Int
    | TickSlow Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            ( { model | list = [] }, Cmd.none )

        AddUuid ->
            ( addId model "", Cmd.none )

        AddUuids n ->
            ( addIds n model, Cmd.none )

        Tick _ ->
            if List.length model.list < 3 then
                ( addId model "", Cmd.none )

            else
                ( model, Cmd.none )

        TickSlow _ ->
            ( addId model " -s", Cmd.none )


addId : Model -> String -> Model
addId model tag =
    let
        ( _, oldSeed ) =
            model.id

        ( newId, newSeed ) =
            Random.step Uuid.uuidGenerator oldSeed
    in
    { model
        | id = ( Just newId, newSeed )
        , list = (Uuid.toString newId ++ tag) :: model.list
    }


addIds : Int -> Model -> Model
addIds n model =
    List.foldl (\_ m -> addId m "") model (List.range 1 n)




-- generate : Int -> (a -> a) -> a -> a
-- generate n fn m =
--     if n <= 1 then
--         fn m
--     else
--         generate (n - 1) f (fn m)
---- VIEW ----


showModel : Model -> Html Msg
showModel model =
    p [] [ div [] (List.map (\v -> div [] [ text v ]) model.list) ]


view : Model -> Html Msg
view model =
    let
        n =
            3
    in
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!! Yes." ]
        , button [ onClick AddUuid ] [ text "add uuid" ]
        , button [ onClick Clear ] [ text "clear" ]
        , button [ onClick (AddUuids n) ] [ text ("add " ++ String.fromInt n ++ " uuid") ]
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



-- subscriptions : Model -> Sub Msg
-- subscriptions _ =
--     Time.every 5000 Tick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Time.every 500 Tick, Time.every 10000 TickSlow ]
