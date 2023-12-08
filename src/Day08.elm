module Day08 exposing (..)

import Dict exposing (..)
import List.Extra as ListExtra

type alias Node = (String, (String, String))
type Direction = Left | Right

parseNode: String -> Node
parseNode str =
    let
       split: String -> (String, String)
       split s =
           case  s |> String.split "," |> List.map (\x -> x |> String.toList |> List.filter Char.isAlpha |> String.fromList ) of
                [a, b] -> (a, b)
                _ -> ("", "")

    in
    case str |> String.split "=" of
        a :: b :: _ -> (a, split b)
        _ -> ("", ("", ""))
--parse String -> String, Dict String (String, String)

loop : List Direction -> (Direction, List Direction)
loop list =
    case list of
        [] -> (Left, [])
        x :: xs -> (x, xs ++ [x])

-- https://adventofcode.com/2023/day/8
--
-- Part 1

