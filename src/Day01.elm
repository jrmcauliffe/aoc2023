module Day01 exposing (..)

-- https://adventofcode.com/2023/day/1
-- Unscramble the calibration document


lineDigit : String -> Int
lineDigit s =
    let
        firstNum i =
            i |> String.toList |> List.filter Char.isDigit |> List.head |> Maybe.withDefault '0'

        front =
            firstNum s

        back =
            s |> String.reverse |> firstNum
    in
    [ front, back ] |> String.fromList |> String.toInt |> Maybe.withDefault 0


lineDigit2 : String -> Int
lineDigit2 s =
    let
        front =
            s |> replacer |> String.toList |> List.head |> Maybe.withDefault '0'

        back =
            s |> replacerR |> String.toList |> List.head |> Maybe.withDefault '0'
    in
    [ front, back ] |> String.fromList |> String.toInt |> Maybe.withDefault 0


translations : List ( String, String )
translations =
    [ ( "one", "1" ), ( "two", "2" ), ( "three", "3" ), ( "four", "4" ), ( "five", "5" ), ( "six", "6" ), ( "seven", "7" ), ( "eight", "8" ), ( "nine", "9" ) ]


reverseTranslations : List ( String, String )
reverseTranslations =
    translations |> List.map (\( a, b ) -> ( String.reverse a, b ))


replacer : String -> String
replacer s =
    let
        firstChar =
            s |> String.toList |> List.head |> Maybe.withDefault ' '
    in
    if String.isEmpty s then
        ""

    else if firstChar |> Char.isDigit then
        String.fromChar firstChar ++ (String.dropLeft 1 s |> replacer)

    else if s |> String.startsWith "one" then
        "1" ++ (String.dropLeft 3 s |> replacer)

    else if s |> String.startsWith "two" then
        "2" ++ (String.dropLeft 3 s |> replacer)

    else if s |> String.startsWith "three" then
        "3" ++ (String.dropLeft 5 s |> replacer)

    else if s |> String.startsWith "four" then
        "4" ++ (String.dropLeft 4 s |> replacer)

    else if s |> String.startsWith "five" then
        "5" ++ (String.dropLeft 4 s |> replacer)

    else if s |> String.startsWith "six" then
        "6" ++ (String.dropLeft 3 s |> replacer)

    else if s |> String.startsWith "seven" then
        "7" ++ (String.dropLeft 5 s |> replacer)

    else if s |> String.startsWith "eight" then
        "8" ++ (String.dropLeft 5 s |> replacer)

    else if s |> String.startsWith "nine" then
        "9" ++ (String.dropLeft 4 s |> replacer)

    else
        s |> String.dropLeft 1 |> replacer


replacerR : String -> String
replacerR s =
    let
        firstChar =
            s |> String.reverse |> String.toList |> List.head |> Maybe.withDefault ' '
    in
    if String.isEmpty s then
        ""

    else if firstChar |> Char.isDigit then
        String.fromChar firstChar ++ (String.dropRight 1 s |> replacerR)

    else if s |> String.endsWith "one" then
        "1" ++ (String.dropRight 3 s |> replacerR)

    else if s |> String.endsWith "two" then
        "2" ++ (String.dropRight 3 s |> replacerR)

    else if s |> String.endsWith "three" then
        "3" ++ (String.dropRight 5 s |> replacerR)

    else if s |> String.endsWith "four" then
        "4" ++ (String.dropRight 4 s |> replacerR)

    else if s |> String.endsWith "five" then
        "5" ++ (String.dropRight 4 s |> replacerR)

    else if s |> String.endsWith "six" then
        "6" ++ (String.dropRight 3 s |> replacerR)

    else if s |> String.endsWith "seven" then
        "7" ++ (String.dropRight 5 s |> replacerR)

    else if s |> String.endsWith "eight" then
        "8" ++ (String.dropRight 5 s |> replacerR)

    else if s |> String.endsWith "nine" then
        "9" ++ (String.dropRight 4 s |> replacerR)

    else
        s |> String.dropRight 1 |> replacerR


lineSum : List String -> Int
lineSum xs =
    xs |> List.map lineDigit |> List.sum


lineSum2 : List String -> Int
lineSum2 xs =
    xs |> List.map lineDigit2 |> List.sum
