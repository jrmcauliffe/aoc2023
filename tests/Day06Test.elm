module Day06Test exposing (suite)

import Day06 exposing (..)
import Expect exposing (equal)
import Test exposing (..)


suite : Test
suite =
    describe "Day 5 Tests"
        [ describe "Part 1"
            [ test "Given Example" <|
                \_ ->
                    example
                        |> part1
                        |> equal 288
            , test "Given Problem" <|
                \_ ->
                    problem
                        |> part1
                        |> equal 3317888
            ]
        --, describe "Part 2"
        --    [ test "Given Example" <|
        --        \_ ->
        --            example
        --                                    |> squash
        --                |> part2
        --                |> equal 71503
        --    , test "Given Problem" <|
        --        \_ ->
        --            problem
        --                                    |> squash
        --                |> part2
        --                |> equal 24655068
        --    ]
        ]


example : List (Int, Int)
example = [(7,9),(15,40),(30,200)]


problem : List (Int, Int)
problem = [(42, 308),(89,1170),(91,1291),(89,1467)]