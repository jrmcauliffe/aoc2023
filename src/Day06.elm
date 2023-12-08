module Day06 exposing (..)

-- https://adventofcode.com/2023/day/6
--
-- Part 1


race : Int -> Int -> Int
race total hold = hold * (total - hold)

combos : Int -> List Int
combos seconds = List.range 0 seconds |> List.map (race seconds)

winRace : (Int, Int) -> Int
winRace (time, record) = combos time |> List.filter (\x -> x > record) |> List.length

part1 : List (Int, Int) -> Int
part1 races = races|> List.map winRace |> List.product

-- Part 2

-- Combine lits of races into one race as per the rules
squash : List (Int, Int) -> (Int, Int)
squash l =
    let
         f (a,b) (c,d) = (String.fromInt a ++ c, String.fromInt b ++ d)
    in
    l |> List.foldr f ("","")
    |> Tuple.mapFirst String.toInt |> Tuple.mapSecond String.toInt
    |> Tuple.mapBoth (Maybe.withDefault 0) (Maybe.withDefault 0)

-- Does this hold and time beat the record?
counts : Int -> Int -> Int -> Int
counts h t r = if (race h t) > r then 1 else 0

factorial : Int -> Int -> Int
factorial r t =
  factorialHelp t r t 0

factorialHelp : Int -> Int -> Int -> Int-> Int
factorialHelp n r t sumSoFar =
  if n <= 1 then
    sumSoFar
  else
    factorialHelp (n - 1) r t (counts n r t + sumSoFar)

part2 : (Int, Int) -> Int
part2 (t, r) = factorial t r

