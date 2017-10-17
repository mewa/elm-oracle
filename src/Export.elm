module Export exposing (Export(..), Case(..), parse)

{-| Parser for things exposed from modules.

@docs Export, Case, parse

-}

import Regex exposing (Regex, find, HowMany(..), Match, regex)
import Set exposing (Set)
import String


{-| Case
-}
type Case
    = AllCase
    | NoneCase
    | SomeCase (List String)


toCase : List String -> Case
toCase cases =
    case cases of
        [] ->
            NoneCase

        [ ".." ] ->
            AllCase

        xs ->
            SomeCase xs


{-| Export
-}
type Export
    = AllExport
    | NoneExport
    | SubsetExport (List Export)
    | FunctionExport String
    | TypeExport String Case


isUpcase : String -> Bool
isUpcase s =
    let
        first =
            String.left 1 s
    in
        first == String.toUpper first


split : Maybe String -> Maybe (List String)
split s =
    Maybe.map ((List.map String.trim) << String.split ",") s


pattern : Regex
pattern =
    regex "(?:(\\w+)(?:\\(([^()]+)\\))?|\\(([^()]+)\\))"


{-| Parse.
-}
parse : String -> Export
parse source =
    if source == "" then
        NoneExport
    else if source == ".." then
        AllExport
    else
        let
            match source_ =
                List.map .submatches (find All pattern source_)

            process exports =
                case exports of
                    name :: types :: binop :: [] ->
                        let
                            name_ =
                                Maybe.withDefault "" name

                            types_ =
                                Maybe.withDefault [] (split types)

                            binop_ =
                                Maybe.map FunctionExport binop
                        in
                            case binop of
                                Just binop_ ->
                                    FunctionExport binop_

                                Nothing ->
                                    if isUpcase name_ then
                                        TypeExport name_ (toCase types_)
                                    else
                                        FunctionExport name_

                    _ ->
                        Debug.crash "Shouldn't have gotten here processing exports."
        in
            SubsetExport (List.map process (match source))
