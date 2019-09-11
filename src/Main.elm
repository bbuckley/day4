module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Dict exposing (Dict, insert)
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random exposing (Seed)
import Time
import Uuid exposing (Uuid)



---- MODEL ----


type Status
    = A
    | T
    | Dis
    | Dth


statuses : List Status
statuses =
    [ A, T, Dis, Dth ]


type CalcType
    = Modeling
    | Final
    | UI1
    | ModelingAB


calcTypes : List CalcType
calcTypes =
    [ Modeling, Final, UI1, ModelingAB ]


toString : CalcType -> String
toString calcType =
    case calcType of
        Modeling ->
            "Modeling"

        Final ->
            "Final"

        UI1 ->
            "UI1"

        ModelingAB ->
            "Modeling"


type alias Tc =
    { status : Maybe Status
    , calcType : Maybe CalcType
    }


tc : Tc
tc =
    Tc Nothing Nothing


type alias Model =
    { id : ( Maybe Uuid, Seed )
    , dict : Dict String Tc
    }


init : Int -> ( Model, Cmd Msg )
init seed =
    ( { id = ( Nothing, Random.initialSeed seed )
      , dict = Dict.empty
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
    | TickSlower Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            ( { model | dict = Dict.empty }, Cmd.none )

        AddUuid ->
            ( addId model "", Cmd.none )

        AddUuids n ->
            ( addIds n model, Cmd.none )

        Tick _ ->
            if Dict.size model.dict < 3 then
                ( addId model "", Cmd.none )

            else
                ( model, Cmd.none )

        TickSlow _ ->
            ( addId model " -s", Cmd.none )

        TickSlower _ ->
            ( addFirstId model " -ss", Cmd.none )


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
        , dict = Dict.insert (Uuid.toString newId ++ tag) tc model.dict
    }


addFirstId : Model -> String -> Model
addFirstId model tag =
    let
        ( _, oldSeed ) =
            model.id

        ( newId, newSeed ) =
            Random.step Uuid.uuidGenerator oldSeed
    in
    { model
        | id = ( Just newId, newSeed )
        , dict = Dict.insert (Uuid.toString newId ++ tag) (Tc Nothing Nothing) model.dict
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
    p [] [ div [] (List.map (\v -> div [] [ text v ]) (Dict.keys model.dict)) ]


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
    Sub.batch [ Time.every 500 Tick, Time.every 10000 TickSlow, Time.every 30000 TickSlower ]
