module Day01 exposing (..)

-- https://adventofcode.com/2023/day/1
-- Unscramble the calibration document
-- Part 1 Find the first and last digit in a string and concatenate them


lineDigit : String -> Maybe Int
lineDigit s =
    (s |> findFirst) ++ (s |> String.reverse |> findFirst) |> String.toInt


findFirst : String -> String
findFirst s =
    let
        firstChar : Maybe Char
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
    xs |> List.filterMap lineDigit |> List.sum



-- Part 2 Now account for 'number words' as well as digits


wordList : List ( String, String )
wordList =
    [ ( "one", "1" ), ( "two", "2" ), ( "three", "3" ), ( "four", "4" ), ( "five", "5" ), ( "six", "6" ), ( "seven", "7" ), ( "eight", "8" ), ( "nine", "9" ) ]


lineDigitPart2 : String -> Maybe Int
lineDigitPart2 s =
    let
        firstDigit : String
        firstDigit =
            s |> findFirstWithWords wordList

        lastDigit : String
        lastDigit =
            s |> String.reverse |> findFirstWithWords (wordList |> List.map (\( a, b ) -> ( String.reverse a, b )))
    in
    firstDigit ++ lastDigit |> String.toInt


findFirstWithWords : List ( String, String ) -> String -> String
findFirstWithWords wl s =
    let
        firstChar : Maybe Char
        firstChar =
            s |> String.toList |> List.head

        -- Helper function to go through the wordlist and maybe find a match
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
                    -- Found a word match
                    Just digit ->
                        digit

                    -- No match, drop a character and keep looking
                    Nothing ->
                        s |> String.dropLeft 1 |> findFirstWithWords wl

        Nothing ->
            -- Empty string, we're done here
            ""


part2 : List String -> Int
part2 xs =
    xs |> List.filterMap lineDigitPart2 |> List.sum
