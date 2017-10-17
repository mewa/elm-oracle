port module Main exposing (..)

import Dict exposing (Dict)
import Html
import Import exposing (Import)
import Module exposing (Module)


main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \_ -> Html.text ""
        }



-- MODEL


type alias Model =
    { query : String
    , imports : Dict String Import
    , modules : List Module
    }


type alias Flags =
    { file : String
    , query : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        imports =
            Import.database (Import.parse flags.file)
    in
        ( Model flags.query imports [], Cmd.none )



-- UPDATE


type Msg
    = Module String


port output : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Module source ->
            ( { model | modules = (Module.parse source) :: model.modules }, Cmd.none )



-- SUBSCRIPTIONS


port modules : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    modules Module
