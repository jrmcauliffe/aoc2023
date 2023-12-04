module Day04 exposing (part1, part2)

import List.Extra as ListExtra



-- https://adventofcode.com/2023/day/4
-- Scratch cards
-- Part 1
-- Parse game string in to a list of picks and a list of winning numbers


parseGame : String -> ( List Int, List Int )
parseGame s =
    let
        games x =
            x
                |> String.toList
                |> ListExtra.dropWhile ((/=) ':')
                |> List.tail
                |> Maybe.withDefault []
                |> String.fromList

        fromStrings y =
            y |> String.split " " |> List.filterMap String.toInt

        numLists =
            s |> games |> String.split "|" |> List.map fromStrings
    in
    ( List.head numLists |> Maybe.withDefault [], List.tail numLists |> Maybe.andThen List.head |> Maybe.withDefault [] )



-- Count how many of the ticket's numbers are in the winning numbers


winCount : ( List Int, List Int ) -> Int
winCount ( myNums, gameNums ) =
    myNums |> List.filter (\n -> List.member n gameNums) |> List.length



-- Convert the number of wins into the score


score : Int -> Int
score g =
    if g == 0 then
        0

    else
        2 ^ (g - 1)



-- Parse all the game strings and total up the score


part1 : String -> Int
part1 s =
    s |> String.split "\n" |> List.map parseGame |> List.map winCount |> List.map score |> List.sum



-- Part 2
-- Recursively keep track of the number of tickets for each game including free tickets


process : ( List Int, List Int ) -> Int
process ( winsCount, ticketsCount ) =
    let
        first =
            List.head winsCount

        total =
            List.head ticketsCount

        -- Given an existing list of ticket counts, add the correct number and multiple
        addTickets : Int -> Int -> List Int -> List Int
        addTickets n i l =
            let
                newTix : Int -> List Int -> List Int
                newTix ii ll =
                    List.repeat ((ll |> List.length) - ii) 0 |> List.append (List.repeat ii n)
            in
            List.map2 (+) (newTix i l) l
    in
    case ( first, total ) of
        ( Just f, Just t ) ->
            t + process ( List.tail winsCount |> Maybe.withDefault [], List.tail ticketsCount |> Maybe.withDefault [] |> addTickets t f )

        ( _, _ ) ->
            0



-- Parse all the game strings and total up number of tickets generated


part2 : String -> Int
part2 s =
    let
        wc =
            s |> String.split "\n" |> List.map parseGame |> List.map winCount

        tc =
            wc |> List.length |> (\x -> List.repeat x 1)
    in
    process ( wc, tc )
