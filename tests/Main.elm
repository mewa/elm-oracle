port module Main exposing (..)

import Test exposing (..)
import Json.Encode exposing (Value)
import Declarations
import Imports


all =
    describe "Test suite"
        [ Declarations.all
        , Imports.all
        ]
    

port emit : ( String, Value ) -> Cmd msg
