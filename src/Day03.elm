module Day03 exposing (..)

import List.Extra as ListExtra



-- https://adventofcode.com/2023/day/3
-- Gear Ratios
-- Part 1


type alias Location =
    ( Int, Int )


type Datum
    = Part Int (List Location)
    | Symbol Char Location


parseFile : String -> List Datum
parseFile s =
    s |> String.lines |> List.map String.trim |> List.indexedMap parseLine |> List.concat


parseLine : Int -> String -> List Datum
parseLine l s =
    s |> String.toList |> parseChar l 0


parseChar : Int -> Int -> List Char -> List Datum
parseChar row col xs =
    let
        nextSymbol =
            xs |> List.head

        getPartNo rr cc ll =
            let
                partString =
                    ll |> ListExtra.takeWhile Char.isDigit |> String.fromList

                partNo =
                    String.toInt partString |> Maybe.withDefault 0

                partLen =
                    String.length partString

                locations =
                    List.range cc (cc + partLen - 1) |> List.map (\x -> ( rr, x ))
            in
            ( Part partNo locations, partLen )
    in
    case nextSymbol of
        Nothing ->
            []

        Just c ->
            if c == '.' then
                [] ++ parseChar row (col + 1) (List.tail xs |> Maybe.withDefault [])

            else if c |> Char.isDigit then
                case getPartNo row col xs of
                    ( part, partLen ) ->
                        part :: parseChar row (col + partLen) (List.drop partLen xs)

            else
                Symbol c ( row, col ) :: parseChar row (col + 1) (List.tail xs |> Maybe.withDefault [])


isNear : Datum -> Datum -> Bool
isNear p s =
    let
        close : Location -> Location -> Bool
        close ( r1, c1 ) ( r2, c2 ) =
            abs (r1 - r2) <= 1 && abs (c1 - c2) <= 1
    in
    case ( p, s ) of
        ( Part pn xs, Symbol _ ( sr, rc ) ) ->
            (xs |> List.filter (close ( sr, rc )) |> List.length) > 0

        ( _, _ ) ->
            False


isIncluded : Datum -> List Datum -> Bool
isIncluded d ds =
    (ds |> List.filter (isNear d) |> List.length) > 0


findValidPartsTotal : List Datum -> Int
findValidPartsTotal all =
    let
        isPart d =
            case d of
                Part _ _ ->
                    True

                _ ->
                    False

        parts =
            all |> List.filter isPart

        symbols =
            all |> List.filter (\d -> not (isPart d))

        partNum d =
            case d of
                Part pn _ ->
                    pn

                _ ->
                    0
    in
    parts |> List.filter (\p -> isIncluded p symbols) |> List.map partNum |> List.sum



-- Part 2


findValidGearsTotal : List Datum -> Int
findValidGearsTotal all =
    let
        isGear d =
            case d of
                Symbol c _ ->
                    c == '*'

                _ ->
                    False

        isPart d =
            case d of
                Part _ _ ->
                    True

                _ ->
                    False

        parts =
            all |> List.filter isPart

        gears =
            all |> List.filter isGear

        partNum d =
            case d of
                Part pn _ ->
                    pn

                _ ->
                    0

        gearRatio : List Datum -> Int
        gearRatio l =
            l |> List.map partNum |> List.product
    in
    gears |> List.map (\g -> parts |> List.filter (\p -> isNear p g)) |> List.filter (\ps -> List.length ps == 2) |> List.map gearRatio |> List.sum
