module Day01 exposing (..)

-- https://adventofcode.com/2023/day/1
-- Unscramble the calibration document
-- Part 1 Find the first and last digit in a string an concatenate them


lineDigit : String -> Int
lineDigit s =
    (s |> findFirst) ++ (s |> String.reverse |> findFirst) |> String.toInt |> Maybe.withDefault 0


findFirst : String -> String
findFirst s =
    let
        firstChar =
            s |> String.toList |> List.head
    in
    case firstChar of
        Just c ->
            if c |> Char.isDigit then
                -- Found the first Digit
                c |> String.fromChar

            else
                -- Drop and keep looking
                s |> String.dropLeft 1 |> findFirst

        Nothing ->
            -- Empty string, we're done here
            ""


part1 : List String -> Int
part1 xs =
    xs |> List.map lineDigit |> List.sum



-- Part 2 Now account for 'number words' as well as digits


wordList : List ( String, String )
wordList =
    [ ( "one", "1" ), ( "two", "2" ), ( "three", "3" ), ( "four", "4" ), ( "five", "5" ), ( "six", "6" ), ( "seven", "7" ), ( "eight", "8" ), ( "nine", "9" ) ]


lineDigitPart2 : String -> Int
lineDigitPart2 s =
    let
        firstDigit =
            s |> findFirstWithWords wordList

        secondDigit =
            s |> String.reverse |> findFirstWithWords (wordList |> List.map (\( a, b ) -> ( String.reverse a, b )))
    in
    firstDigit ++ secondDigit |> String.toInt |> Maybe.withDefault 0


findFirstWithWords : List ( String, String ) -> String -> String
findFirstWithWords wl s =
    let
        firstChar =
            s |> String.toList |> List.head

        -- Helper function to go through the wordlist and find the first match from the wordlist
        findWord : String -> Maybe String
        findWord ss =
            wl
                |> List.filterMap
                    (\( a, b ) ->
                        if String.startsWith a ss then
                            Just b

                        else
                            Nothing
                    )
                |> List.head
    in
    case firstChar of
        Just c ->
            if c |> Char.isDigit then
                -- Found the first Digit
                c |> String.fromChar

            else
                -- Try and find a word instead
                case findWord s of
                    Just digit ->
                        digit

                    Nothing ->
                        s |> String.dropLeft 1 |> findFirstWithWords wl

        Nothing ->
            -- Empty string, we're done here
            ""


part2 : List String -> Int
part2 xs =
    xs |> List.map lineDigitPart2 |> List.sum
