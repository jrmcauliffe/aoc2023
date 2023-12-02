module Day02 exposing (gamePossible, minGamePower, parseGame)

-- https://adventofcode.com/2023/day/2
-- Cube Conundrum
-- Part 1


type Cube
    = Red
    | Green
    | Blue


type alias Pull =
    ( Int, Cube )


type alias Turn =
    List Pull


type alias Game =
    { num : Int
    , turns : List Turn
    }



-- Parse a Cube colour and count from a string


parsePull : String -> Pull
parsePull s =
    let
        strip : String -> Int
        strip ss =
            ss |> String.filter Char.isDigit >> String.toInt >> Maybe.withDefault 0
    in
    if String.contains "red" s then
        ( strip s, Red )

    else if String.contains "green" s then
        ( strip s, Green )

    else
        ( strip s, Blue )



-- Parse a group of pulls from a string


parseTurn : String -> Turn
parseTurn s =
    s |> String.split "," |> List.map parsePull



-- Parse a group of turns from a string


parseTurns : String -> List Turn
parseTurns s =
    s |> String.split ";" |> List.map parseTurn



-- Parse a game from a string


parseGame : String -> Game
parseGame s =
    let
        spl =
            s |> String.split ":"

        gameNum =
            spl
                |> List.head
                |> Maybe.withDefault ""
                |> String.toList
                |> List.filter Char.isDigit
                |> String.fromList
                |> String.toInt
                |> Maybe.withDefault 0

        turns =
            spl |> List.drop 1 |> List.head |> Maybe.withDefault "" |> parseTurns
    in
    { num = gameNum, turns = turns }



-- Check to see if a game is possible with the given number of cubes


gamePossible : Int -> Int -> Int -> Game -> Maybe Game
gamePossible red green blue game =
    let
        pulls =
            game.turns |> List.concat

        max colour =
            pulls |> List.filter (\( _, c ) -> c == colour) |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0
    in
    if red >= max Red && green >= max Green && blue >= max Blue then
        Just game

    else
        Nothing



---- Part 2
-- Return the product of the minimum number of cubes in the bag of each colour for the given game


minGamePower : Game -> Int
minGamePower game =
    let
        pulls =
            game.turns |> List.concat

        rmax =
            pulls |> List.filter (\( n, c ) -> c == Red) |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0

        gmax =
            pulls |> List.filter (\( n, c ) -> c == Green) |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0

        bmax =
            pulls |> List.filter (\( n, c ) -> c == Blue) |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0
    in
    rmax * bmax * gmax
