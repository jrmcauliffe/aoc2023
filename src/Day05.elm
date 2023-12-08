module Day05 exposing (..)

import Dict exposing (..)
import List.Extra as ListExtra



-- https://adventofcode.com/2023/day/5
--
-- Part 1

type alias Seeds = List Int
type alias Range = (Int, Int, Int)
type alias Map = List (Range)

parseSeeds : String -> Seeds
parseSeeds s =
    s |> String.split ":" |> List.reverse |> List.head |> Maybe.withDefault ""
      |> String.split " " |> List.filterMap (String.toInt)


parseMapLine : String -> Map
parseMapLine s =
         case s |> String.split " " |> List.filterMap (String.toInt) of
         dest :: src :: len :: [] ->
                [ ((src, dest, len)) ]
         _ -> []

parseMap : List String -> Map
parseMap s =
    s
        |> List.map parseMapLine |> List.concat


isNotInts : String -> Bool
isNotInts ss = ss |> String.split " " |> List.filterMap String.toInt |> List.isEmpty


parseMaps : List String -> List Map
parseMaps s =
    case s of
        _ :: rest ->  -- Ignore name
             case rest |> ListExtra.splitWhen isNotInts of
                    Just (thisMap, more) -> parseMap thisMap :: parseMaps more
                    _ -> parseMap rest :: []
        _ -> []

parseProblem: String -> (Seeds, List Map)
parseProblem s =
   case s |> String.lines |> List.filter (\ss -> not (String.isEmpty ss)) of
    seeds :: maps ->
        (parseSeeds seeds, parseMaps maps)
    _ -> ([], [])

part1 : (Seeds, List Map)-> Int
part1 (seeds, maps)  =
    let
        resolveRange : Int -> Range -> Maybe Int
        resolveRange s (src, dest, len) =
            if s >= src && s < (src + len) then
                Just (s - src + dest)
            else
                Nothing
        resolveSingle : Int -> Map -> Int
        resolveSingle s m = m |> List.filterMap (resolveRange s) |> List.head |> Maybe.withDefault s
        resolveMulti : Int -> List Map -> Int
        resolveMulti s mps = case mps of
            m :: rest -> resolveMulti (resolveSingle s m) rest
            _ -> s
    in
    seeds |> List.map (\s -> resolveMulti s maps) |> List.minimum |> Maybe.withDefault 0



-- Part 2

-- Now have ranges of seeds

type alias SeedRange = (Int, Int)

parseSeedRange : List Int -> List SeedRange
parseSeedRange ss =
    case ss of
        src :: len :: rest ->
            (src, len)  :: (parseSeedRange rest)
        _ -> []

resolveSeedRange : Range -> SeedRange -> (List SeedRange, List SeedRange) -- ( No matches, Matches)
resolveSeedRange   (src, dest, len) (inSrc, inLen) =
    let
        endsBefore = (inSrc + inLen - 1) < src
        startsAfter = (inSrc) > (src + len - 1)
    in
    -- Nothing in Range, pass through
    if (endsBefore || startsAfter) then ([(inSrc, inLen)], [])
    -- All in Range, move
    else if inSrc >= src && ((inSrc + inLen) <= (src + len)) then ([], [(dest +(inSrc - src), inLen)])
    -- Partially below, split and reprocess
    else if inSrc < src then
       let
           unprocessed = (inSrc, src - inSrc)
           out = case resolveSeedRange (src, dest, len) (src, inLen - (src - inSrc)) of
               ([], y :: [])  -> ([unprocessed] , [y])  --  found matches in remainder
               (x :: [], y :: [] )  -> ([ unprocessed, x] , [y])   -- Partial match
               _ -> ([], [])

       in
       out
    -- Partially above split and reprocess
    else
      let
          unprocessed = (src + len , (inSrc+inLen) - (src+len))
          out = case resolveSeedRange (src, dest, len) (inSrc, src+len - inSrc) of
                         ([], y :: [])  -> ([unprocessed] , [y])  --  found matches in remainder
                         (x :: [], y :: [] )  -> ([ unprocessed, x ] , [y])   -- Partial match
                         _ -> ([], [])

      in
      out


resolveSeedsMap : List SeedRange -> Map-> List SeedRange
resolveSeedsMap ls m =
    let
        -- Check a seed range against a map
        resolveSeedRangeMap :  Map -> SeedRange -> List SeedRange
        resolveSeedRangeMap mm sr =

            case mm of
                x :: [] ->
                    case resolveSeedRange x sr of
                        (unprocessed, matches) ->
                           matches ++ unprocessed
                x :: xs ->
                    case resolveSeedRange x sr of
                        (unprocessed, matches) ->
                           matches ++ (unprocessed |> List.concatMap (resolveSeedRangeMap xs))

                _ -> []
    in
    ls |> List.concatMap (resolveSeedRangeMap m)


inside : List SeedRange -> List Map -> List SeedRange
inside seeds maps =
    case maps of
        x :: [] -> resolveSeedsMap seeds x
        x :: xs -> inside (resolveSeedsMap seeds x) xs
        _ -> []


show: List SeedRange -> List Map -> List (List SeedRange)
show seeds maps =
    case maps of
        x :: [] -> [resolveSeedsMap seeds x]
        x :: xs -> (resolveSeedsMap seeds x) :: show (resolveSeedsMap seeds x) xs
        _ -> []

part2 : (Seeds, List Map)-> Int
part2 (seeds, maps) =
    let
        pseeds = seeds |> parseSeedRange
    in
    inside pseeds maps |> List.map Tuple.first |> List.minimum |> Maybe.withDefault 0


