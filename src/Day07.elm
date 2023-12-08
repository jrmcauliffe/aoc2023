module Day07 exposing (..)

import List.Extra as ListExtra
import Dict


-- https://adventofcode.com/2023/day/5
--
-- Part 1
type alias Card = Char
type alias Hand = List Card
type alias Wager = Int
type alias Game = (Hand, Wager)
validCards : List Char
validCards = [ '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A' ]
cardVals : Dict.Dict Char Int
cardVals = validCards |> List.indexedMap (\aa bb -> (bb, aa)) |> Dict.fromList


type HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard

parseGame : String -> Game
parseGame s =
    let
        tokens = s |> String.words
    in
    case tokens of
        [ game, wager] -> (game |> String.toList, String.toInt wager |> Maybe.withDefault 0)
        _ -> ([], 0)

handType : Hand -> HandType
handType hand =
    let
        f = ListExtra.frequencies hand |> List.map Tuple.second |> List.sort |> List.reverse
    in
    case f of
        [5] -> FiveOfAKind
        [ 4, 1 ] -> FourOfAKind
        [ 3, 2 ] -> FullHouse
        [ 3, _, _ ] -> ThreeOfAKind
        [ 2, 2, _ ] -> TwoPair
        [ 2, _, _, _ ] -> OnePair
        _ -> HighCard

rankCard : Dict.Dict Char Int -> Card -> Int
rankCard vals card =
        vals |> Dict.get card |> Maybe.withDefault 0

rankHandType : Hand -> Int
rankHandType hand =
    let
        ranks = [ HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind ] |> List.indexedMap Tuple.pair
        ht = handType hand
        typeRank = ranks |> List.filter (\(i, t) -> t == handType hand) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 0
    in
    typeRank

compareHands : Hand -> Hand -> Order
compareHands h1 h2 =
    let
       typeRank1= rankHandType h1
       typeRank2 = rankHandType h2
    in
    if typeRank1 == typeRank2 then -- Same hand type, first card wins
          List.map2 (\a b -> compare (rankCard cardVals a) (rankCard cardVals b)) h1 h2 |> ListExtra.dropWhile (\o -> o == EQ) |> List.head |> Maybe.withDefault EQ
    else compare typeRank1 typeRank2 -- Different hand type, hand type wins

compareGames : Game -> Game -> Order
compareGames (h1, _) (h2, _) = compareHands h1 h2

part1 : String -> Int
part1 input =
    let
        games = input |> String.lines |> List.map parseGame
    in
    games |> List.sortWith compareGames |> List.indexedMap (\i (h, w) -> (i + 1) * w) |> List.sum

-- Part 2

withOutJ : List Card
withOutJ = validCards |> List.filter ((/=) 'J')
newCardVals = 'j' :: withOutJ |> List.indexedMap (\aa bb -> (bb, aa)) |> Dict.fromList

compareHandsWithWild : Hand -> Hand -> Order
compareHandsWithWild h1 h2 =
    let
       typeRank1= rankHandTypeWild h1
       typeRank2 = rankHandTypeWild h2
    in
    if typeRank1 == typeRank2 then -- Same hand type, first card wins
          List.map2 (\a b -> compare (rankCard newCardVals a) (rankCard newCardVals b)) h1 h2 |> ListExtra.dropWhile (\o -> o == EQ) |> List.head |> Maybe.withDefault EQ
    else compare typeRank1 typeRank2 -- Different hand type, hand type wins

compareGamesWW : Game -> Game -> Order
compareGamesWW (h1, _) (h2, _) = compareHandsWithWild h1 h2

rankHandTypeWild : Hand -> Int
rankHandTypeWild hand =
    let
        ranks = [ HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind ] |> List.indexedMap Tuple.pair
        typeRank h = ranks |> List.filter (\(i, t) -> t == handType h) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 0
        ht = bestHand2 hand |> typeRank
    in
    ht


bestHand2 : Hand -> Hand
bestHand2 hand =
        case List.partition ((==) 'J') hand of
        (allJs , []) -> allJs
        ([] , _) -> hand
        ( js , others) ->
            let
                freqs = others |> ListExtra.frequencies |> List.sortBy Tuple.second |> List.reverse
                most = freqs |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 'X'
                noJs = js |> List.length
             in
             (most |> List.repeat noJs) ++ others








part2 : String -> Int
part2 input =
    let
        games = input |> String.lines |> List.map parseGame
    in
    games |> List.sortWith compareGamesWW |> List.indexedMap (\i (h, w) -> (i + 1) * w) |> List.sum

expandWilds : Hand -> List Hand
expandWilds hand =
    case List.partition ((==) 'J') hand of
    ([] , _) -> [ hand ]
    ( x :: [], rest ) -> withOutJ |> List.map (\c -> c :: rest)
    ( x :: xs, rest ) -> withOutJ |> List.concatMap (\c -> expandWilds (c :: xs ++ rest))