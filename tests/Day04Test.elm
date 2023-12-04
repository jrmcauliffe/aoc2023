module Day04Test exposing (suite)

import Day04 exposing (part1, part2)
import Expect exposing (equal)
import Test exposing (..)


suite : Test
suite =
    describe "Day 4 Tests"
        [ describe "Part 1"
            [ test "Given Example" <|
                \_ ->
                    example
                        |> part1
                        |> equal 13
            , test "Given Problem" <|
                \_ ->
                    problem
                        |> part1
                        |> equal 21485
            ]
        , describe "Part 2"
            [ test "Given Example" <|
                \_ ->
                    example
                        |> part2
                        |> equal 30
            , test "Given Problem" <|
                \_ ->
                    problem
                        |> part2
                        |> equal 11024379
            ]
        ]


example =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
       Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
       Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
       Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
       Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
       Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""


problem =
    """Card   1: 30 48 49 69  1 86 94 68 12 85 | 86 57 89  8 81 85 82 68  1 22 90  2 74 12 30 45 69 92 62  4 94 48 47 64 49
       Card   2: 57 32 92 73 76 62 11 19 61 90 | 19 82 53 87 57 80 69 76 90 56 11 61 30 92 73 99  4 32 33 64 34 62 27 78 65
       Card   3:  6 56 40  1 47 26 25 87  4  2 | 12 26 32 25  8  4 41 54 69 99  2 45 70  6 59 23 47  7 49 17  1 56 92 87 40
       Card   4: 26 49 44 82 25 43 47 74 97 13 | 76 62 13 82 55 26 93 84 83 19 47 22 49 44 25 43  7 18  9 45 97 15 90 85 74
       Card   5: 88 58 96 80 56 16 55 13  3 40 | 20 57 23 71 76 43 36 72 52 18 60 28 80 84 64 75 93 46 19 69 25 31 58 45 47
       Card   6: 89 88 34 62 60 41 15 42 57 58 | 58 49 82 42 70 78 72 57 77 47 62 89 30 60 96 98 54 66 25 14  6 34 15 41 88
       Card   7: 97 73 29 26 91 16 66 64 70 23 |  3 12 61 11 70 59 71  1 26 44 14 64 63 93 20 17 89 16 43 96 52 92 46 97 85
       Card   8:  9 33 46 87 34 97 71 30  5 43 | 21 64  8 43 77  9 20 46 10 19 53 33 71 56 39 87 34 50 72 91 30 89 13  5 97
       Card   9: 72 41 12 58  8 56 11 82 22 66 | 27 97 48 14 77 37 33 91 85 39 75 42 58  2 52  9 70 45 72 62  5 21 23 60 99
       Card  10:  3 46 56  2 58  4 92 80 86 52 | 15 56 91 57 61  8 87 34 51  3 23 41 96 45 48 49 43 25 26  9 58 50 52 38 12
       Card  11: 76 94  4 55 97  8 22 99 80 70 | 64 75 23 49 42  1 66 54 85 29 28 94 22 93  4 69 72 62 41  2 86 83 97 34 82
       Card  12: 65 91  7 24 80 20 46 29 58 57 | 51 59 52 63 28 44 62 35 46 49 24 14 58 91 65 78 84 20 43 32 23 15 29 13 22
       Card  13: 98 94 55 43 83 72 19 46 45 22 | 55 67 59 91 46 37 24 52 98  4 72 22 44 94 88 43 27  7 75 13 45 82 15 65 19
       Card  14: 92 51 81  6  5 91 73  9 80 94 | 56 72 71 32 92 98 15  2 10 91 62  1 51 74 69 89 33 96 39 97 21 86 65 52 83
       Card  15: 84 40 87 91  3 61 20 73  2 37 | 77  3 37 81 20  9 51  2 87 73 53 66 26 36 69 99 83 96 88  5 91 18 40 57 80
       Card  16: 97 63 52 67 86 87 91 25 69 43 | 68 67 75  3 85 10 88 99 52 43 95 92 80 87 11 13 96 58  1 17 74 20 36 93 77
       Card  17: 28 90 77 78 52  7 94 13  6 88 | 11 69 49 56 59 14  7 24 53 21 67  2 17  8 47 66 48 30 79 43 86 64 46 28 23
       Card  18: 50 22 25 69 54  4 37 14 49 75 | 82 84 78 88 87 15 80  1 11 91 89 93 25 98 74 42  6 59 23 58 13 46 49 32 95
       Card  19: 96 56 86 38 29 43 69 66 26 46 | 80 13 90 30 41 86 44 95 71 23 40 75 33 14 56 59 89 18 92 79 15  4 84 68 82
       Card  20: 95 26 31 69 63 25 56 87 74 48 | 88 16 42 22 30 14 71 62 35 87 99  3 96 54 41 83 40 29 79 20 50 24 53 10 66
       Card  21: 75 76 94 26 24 56 34 35 93 87 | 42 23 71 55 44 79 12  4 78 32 82 53 40 35 43 50  9 65 29 57 93 86  7 67 19
       Card  22: 53 58 25 33 42 98 94 95 97 22 | 44 91 40  1 88 31 61  6 54 52 47 93  3 99 27 59 15 53 56 39 66 43 51 41 48
       Card  23: 34 95 33 45 56 37 30 94 76 67 |  9  1 72 68 51 66 36 24 21 92 35 50 62 86 90 49 82 46 15  4 70 40 75 64 73
       Card  24: 73 93 49 22 86 18 31 87  3  2 | 27 42 46 97 49 31 47 25 93 74  9 60 44 87 15 63 18  2 73 89  3 62 86 65 22
       Card  25: 36 24 95 21 31  7 56 93 82 17 | 82 36 23 54 66 83 95 31 65 16 17 86  7 21 61 24 55 56 13 53 59 64 96 39 32
       Card  26: 16 89 92 12 40 17  3 80 20 21 | 37 72 26  9 79 55 74  2 92 59 17 35 71 94 98 31 58  3 32 51 21 88 40 96 57
       Card  27: 70 19 81 48 62 26 45 86 65 41 | 63 38 71 43 50 42 25 14 18 54 66 96 29 36 95 73 20 76 16  6 58 78 56 87 91
       Card  28: 61 66 57 75 52 20 41 21 24 14 | 83 51 77 14 66 91 75  3 47 20 18 65 76 52  5 11 72 57 10 36 30 92 73 28 19
       Card  29: 94 39 68 79 28 70 81 97 59 48 | 46  4 41 39 38 75 18 76 45 31 79 97 91 64 71 85 63 55  8 94 67 59 36 74 49
       Card  30: 90 45 15 47  1 24 99 62 69 55 | 42  5  1 37 77 71 66 57 94 18 26 47 46 27 48 80 24 95 30 15 53 90 38 99 69
       Card  31: 53 16 59 23 78 95 38 17 40  7 | 86 20  7 98 44 19 73 36 40  4 58 16 94  2 83 32  6 78 57 13 35 30 87 51  5
       Card  32: 64 79 78  6 24 15 44 45 82 33 | 34 18 80 94 54 59 14 23 77 25 73 81 26 62 17 45 37 82 67 24 98 78 85 43 13
       Card  33: 91 97 83 95 89 32 86 94 96 19 | 93 39 57 58 41 62  1 80 33  2 24 51 27 25  6 10 78 76 63 65 38 79 61 98 90
       Card  34: 41 84 69 81 95 71  7 80 65 55 | 88 17 73  3 54 66 68  8 76 84 36 79 55 47 58 91 37 53 57 46 93 71 83 42 12
       Card  35: 51 67 16 48 34 86 59 74 98 65 | 14 75 23 98 48 83 11 35 18 93 68 50 85 31 40 82 27 92 55 63 64 37 76  2 33
       Card  36: 26 80 18 67 87 93 79  1 59 61 | 83 51 97 34 70 90 49 44 39 95 92 38 55  9 65 96 94 82 61  1 20 46 50 63  6
       Card  37: 71 13 91 66 24  3  1 41 27 75 | 12 38 45  5 80 11  8 59 90 64 85 49 52 56 35 96  7 20 53 39 44 22 76 57 66
       Card  38: 21 56  2 42 51 28 37 38 95 12 |  3 84 55 81 91  6 90 14 68 93 64 33 78  7  4 60 18 54 11 29 26 19 75 72 52
       Card  39:  1 40 97 60 90 47 74 41  7 58 | 43  8 47 72  7 78 41 96  5 37 98 97 58 89 23 65 74  1 92 90 28 16 60 40 67
       Card  40: 75 99 14 36 64 42 30  8 52 97 | 64 89 36 80 99 30 57 40 35 75 53 97 52 38 68  1 74 42 14 60 49 79  8 72 20
       Card  41: 26 38 87 47 39 62 56  2 49 51 | 25 45 87 63 24  2 39 51 54 56 70 62 77 57 68 49 31 34 26 72 42 38 47 94 29
       Card  42: 26 69 10 47 89 27 43 62 97 50 | 62 79 50 69  2 97 68 87 27  5 67 18 64 26 10 25 91  4  7 43 70 14  1 54 89
       Card  43: 92 16 95 21 15 77 27 88 76 63 | 92 77 78  1 58 96  8 31 63 16 14  2 54 88 15 52 73 76 21 27 84 71 25 95 72
       Card  44: 27 31 64 16 96 95 74 57 59 53 | 75 20 48 11 18 44  3 45 79 98 12 54 30 60 32 15 13 72 21 87 85 52 70 33 35
       Card  45: 62 31 89 91 85 83 93 11 26 23 | 53 55 71 62 14 65  8 21 96 57 17 91 51 82 86 47 64 80  3 69 56 90 85 32 70
       Card  46: 52 67 93 77 39 13 55 63 68 78 | 19 96 76 50 11 61 72 81 64 10 84 22 83 37  3 58 80 48 57 18 59 62 92  4  9
       Card  47: 31 14 25 56 83 48 53 64 39 77 | 48 34 27 16  5 55 77 42 17  6 24 49 37 92 51  8 97 52 41 11 31 80 47 87 38
       Card  48: 69 65 89 40  1 31 64 50 48 38 | 44 75 74 58 13 56 50 80 22 89 47 95  8 53 31 38 48 64 72 39  1 52 43 17 65
       Card  49: 30 49 86 87 78 96 73 44 21 90 | 14 39 75 49 96 63 56 73 64 81 93 98 99  1 13 65 54 88 61 83 44 30 45 15 67
       Card  50: 61 79 65 67 77 58 21 37 90 48 | 81 79 62 51 21 85 61 90 92 47 29 23 80  7 98 40 59 87 18 77 48  6 99 88  1
       Card  51:  9 45 35 38 92 47 27 61 91 67 | 75 86 50 74 41 92 59 71 52 91 97 73 96  4 19 87 69 76 98 21 90 37 32 14 77
       Card  52: 82 80 79 33 18 70 52 73 38 39 | 21 82 95 25 66 89 75 85 84 39 47 28 49 45 79 17 56 10 29 52 44 77 70 32 65
       Card  53:  2 47 76 33 17 62 13 70  9 31 | 84  2 31 47 13 97  4  3 33 21 17  8 62 10 19 78 41 70 43 98 79 59 95 29 99
       Card  54: 50 10 89 77 41 82 43 84 30  7 | 28 49 35 76  9 23 65 71 60 36 32 15  3 61 95 10 24 77 17 97 48 62 88 51 68
       Card  55: 98 86  8 43 54 59 11  7 63 42 | 24 98  1 58 51 68  2 56  8 54 29 25 66 17 59 86 30 48 43 60  5 55 18 44 28
       Card  56: 88 38  8 43 91 12 69 45 71 20 | 11 34  1 95 77 19 39 40 33  5 80 51 75 94 84 23 82  8 86 62 74 89 96 22 66
       Card  57: 95 38 81 99 57 68 60 55 39 18 | 34 38 45  7 63 12 97 35 44 17 57 58 92 41 89 30 39 47 66 10 82 52 68 55 98
       Card  58: 34 72 18 15 20  1 28 89  7 35 | 14 22 61 36 84 26 55 62 32 53 44 51 46 17  6 54 85 35 95 40 10 92 42 19 83
       Card  59: 58 43 86  3 73 17 24 49 29  5 | 87 93 56 60 35 48 86  8  7 16 67 89 73 43 69 31 88 23 50 59 45 84 61 26 65
       Card  60:  4 74 59 28 17 23 67 48 82 20 | 76 91 88 47 84  2 14 61 56 11 58 34 24 99 75 66 33 96 92 94 64 73 72 45  7
       Card  61: 24 40  2 95 58 17 28 43  4  8 | 97 77 49 15 51 41 70 80  6 74 11 68 38  5  8 64 81 30 89 72 63 52 39  7 82
       Card  62: 79 22 90 64 37 95  4 87 30 38 | 73 25 91 13 83 45 77 53 88 63 43 92 78 34 32 58 24 40 50 61 55 41  2 67 14
       Card  63: 70 13 43 35 58 84 44 51 65 73 | 43  3 48 51 69 70 31 98 19 15 91 61 55 39 41 58 44 65 35 13 25 73  9 50 56
       Card  64: 85 64 44 45 31 95  3 16 19 74 | 53 13  9 89 83 58 47 90 77 59 98 99 74 91 52 15 61 80 75 88 54 23 24  8 29
       Card  65: 70 33 34 72 80 56  7 95 71 32 | 16 32 56 34 73 31 45 29 71 33 98 72 97 19 39 94 91 90 70 75 40  7 95 80 96
       Card  66: 29 30 35 39 52 17 25  1 62 79 | 83  2 69 13 36 76 59 50 90 64 30 37 48 75 95 25 26 60  7 29 86 42  8 22 11
       Card  67: 51 59 72 52 60 74 20 68 90 40 | 51 34  8  2 67 46 47 10  9 43 61 56 28 74 73 60 81 25 87  4 17 84 65 42 32
       Card  68: 36  3 98 39  7 74 66 85 81 14 |  7 78 14 74 85 97 55 62 66 98 28 94 39 81 42  6 24 36 60 50 75 87  3  9 18
       Card  69: 72 99 53 87 83 54 58 32 79 26 | 54 70  5 34  1 79 26 99 90 11 77 13 98 83 15 58 40 96 91 12 32 53 87 72 43
       Card  70: 72 26 89 44  9  7 14 95 46 51 | 45 44 91  3 71 77 82 56  2  6 24 20 49 75 60 22 58 80 42 68 32 54 76 29 79
       Card  71: 56 75 58 97  4 95 16 23  7 71 | 63 96 40 64 42 38 65 48 78 35  5 24 20 41 70 89 45 17 49 21 10 44 92 60 51
       Card  72: 82  3 37 72 19 78 84 69 43 62 | 95 43 97 82 72 14 70 47 45 26 92 77 67 62 49 22 19  3 37  7 69 78 94 84  6
       Card  73: 63 77 50 12 46 80 13 54 64 24 | 60 50 17 87  1 46 42 13 80 63 82 48 45 35 55 29 30 31  9 24 64 27 77 40 69
       Card  74: 66 73 38  5 89  1 11 10 91 92 | 89 23 28 50 82 63 33 91 72 74 73 66 24 17 42 11 36 94 79 92  4 85 29 57 10
       Card  75: 66 51 34 46 78 27 89 42 52  4 | 87 79 28 18  1 35 81 60 65 76 56 78 42 43 47 45 89 70  4 68 19 63 33 94 86
       Card  76: 35 43 54 87 15 71 30 92 29 24 |  6 76 36 34 50 47 88 29 44 73 68 97 87 24 81 20 94 86 70 90 71  2 15 98 54
       Card  77: 27 77 81 31 53 47 45 71 73 41 | 85 40 63 68 22 10 75 55 62 67 54 49 51 79 17 92 93 21 73 38 37 74 90 23 98
       Card  78: 79  1 93 23 31 48 32 53 64 57 | 52 76 35 68  3 75 22 81 79 51 80 46 88 65 83 44 67 89 31 86  8 98 97 16 30
       Card  79: 72 80  7 74 10 22 12 34 89 97 | 47 62  5 23 17 27  9 21 78 68 66 98 29 39 51 83 24 90 64 10 48 22 97  2 35
       Card  80: 80 94 30 12 28 54 78 34 58 63 | 46 33 86 44 50 94 96 66 57 58 87 65 92 71 32 56 73 90 11 85 21 76 45 27 64
       Card  81: 94 60 72 30 14  3 40 86 69 82 | 74 99 11 55 27 54 70 46 89 24 44 85  6 53 58 15 26 10 20 38 56 63  1  4 12
       Card  82: 88 30 84 71 61 63 20 56 49 89 | 10 87 14  2  9 57 46 27 12 41 78 59  5 55 19 94  7 24  1 25 13 28 42 33 68
       Card  83:  6 35 37 64  9 44 21 42 56 30 | 57 69 83 89 47 74 95 71 81 84 41 45 40  2  1 77 32 75 90 60 62 82 79 22 55
       Card  84: 34 13  4 21 88 26 18 96 38  3 | 16 66 63  9  7 95 28 72 74 41 10 85 30 70 91 69 47 44 33 49 60 46 57 62 36
       Card  85: 94 58 24 74 61  5 49 99 30 54 | 99 11 61 58 25  4  2 55 30 75 62 51 94 22  3 36 54 84 71  9  8 24 70 90  5
       Card  86: 33 82 19 66 52 24 76 34  5 84 | 82 37 63 83 36 22 13 76 84 19 38  6 65 33 66 40 95 52 24 45  5 88 34 89 49
       Card  87: 86 24 92  7 72 28 63 23 12 82 |  9 59 36 56 40  1 76  4 88 10 85 14 17 15 61 29 57 50 98 49  3 41 42 93 22
       Card  88: 23 32 78 34 12  8 89 20 50 29 | 59 24 13 72 14  7 83 37 32 39 90 46 96 49 94 82 43 33 85 92 69 62 57 48  9
       Card  89: 98 48 90 71 56 21 61 86 29 63 | 50 62  2 23 70 44 36 35 57 48 13 64 21 90 93  1 53  5 33  7 29 67 38 61 54
       Card  90: 27 66 35 40 54 68  1 80 42 49 | 97 94  2  1 66 55 15 49  5 75 26 30 20 25 73 79 10 40 57 85 23 43 68 38 14
       Card  91: 52 74 25 22 84 68 76 27 89 13 | 52 75 90 22 73 94 64 72 84 70 74 67 30  2  1 29 54 86 25 58 41  6 43 91 34
       Card  92: 35 83 75 46 37 28 82 54 13 77 | 96 77 46 33 37 59 62 13 28 54 75 86 74 35 47 85  6 80 55 20 36 83 38 82 65
       Card  93:  8 19 27 84 17 42 23 22 78 16 | 76 16 63 78 89  1 27 57 83 34 36 69 42 95 41 44 74 28 79 39  4 92 19 23  7
       Card  94: 90 53 59 26 63 91 50 81 69 85 | 20 78 12 23 31 91 42 36 75 95 19 76 96 49 94 47 63 53 27 88 67 69 85 60  7
       Card  95: 87 25 89 31 63 32 13 80 60 14 | 95 82 78 34 87 13  3 60 88 28 96 59 36 77 14 79 55 35 83 25 73 58 48 63 86
       Card  96: 32 27 21 77 35  9  8 28 97 82 | 53 97 29 81 66  3 17 85  8 35 56 38 26 60 49  9 39 86 28 71 89 88 54 44 27
       Card  97:  2 42 48 43 96 55 51 60 33 89 | 85  3 31 88 98 25 41 81 44 68  4 80 63 17 45 57 89  7 38 76 95 53 49 67 13
       Card  98: 63 57 83 58 23 27 92 21  2 76 | 58 76 95 80 62 56 98 33 59 46 51 23 93 69 96 48 36 53 19  8 45 32 42  2 89
       Card  99: 28 85 31 55 69 73 70 66 11 79 | 22  4 83 55 26 20 29 72 54 12 30 99 66 14 10 17 15  8 88 45 76 69 58 89 75
       Card 100: 99 91 55 84 81 61 94 95  8 25 | 30 15 52 88 27  6 53 41 45 95 66 19 98 86 26 31 54 43 51  9 82 97 57 60 16
       Card 101: 86 47 76 89 85  1 34 35 53 51 | 81 13 69 14 79 68 37 39 63 59 50 15 56 99 92 80 36 43 18 77 65 19 45 91  8
       Card 102: 98 27 49 51 14 77  6 41 97 64 | 60 76  3 79 48 42 29 37 10 24 74 62 11 17 95 73 56 85 28 38 90 63 84 30 94
       Card 103: 75 45 64 88 40 69 96 66 52 28 | 28 12 88 45 60 64 16 96 41 17 82 97 24 75 54 67 92 14 40 85 93 52 31 37 66
       Card 104: 36 69 82 51 81 42 92 95 62 73 | 13  5 65 87 70 36 95 99  9 62 24 55 31 42 51 82  7 34 69 92 14 18 79 81 73
       Card 105: 87 81 71 62  3 41 17 86 88  5 | 98 31 80 39 25 35 47 42 95 14 91 83 65 12 87 96 88 58  5 19 56  1 97 62 48
       Card 106: 31 55 80 86  7 20 37  2 89  4 | 70 20 92 62 38 86  7 93 79  6 48 37 19 21 89 47 25 78 43 46 54 69 63  8 80
       Card 107: 99 85  5 37 34 95 11 12 51 96 | 69 64 67 53 15 81 89  3  2 54 49 98 71 72  1 76 91  8 17 51 23 83 21 86 92
       Card 108: 95 57 98 30  3 83  6 64 52 18 | 59 88 91 78 61 30 52 98 65 68 25 17 13  3 64 89 57 37 83  6 95 36 97 20 18
       Card 109: 97 99 89 96 69 84 62 66 61 83 |  9 31 22 49 96 84 17 61 15 99 39 89 26  4 92 32 97 21 62 56 66 82 69 83 13
       Card 110: 61 93 64 41 91 76 74 21 56  5 | 51 65 17 29 19 45 70 87 40 10 62 20 89 85 53 36 42 37 50  2 74 56 64 46 48
       Card 111:  8 85 35 16 52 31 94 10 29 82 | 70 60 11  6 31 58 10 52 43 35 16 30 94 62  1 29 74  9  4 90  7 59 40 20 85
       Card 112: 96 15 95 19 76 41 77 62 46 89 | 61 35 99 56 60 82 17 16 47  4 67 20 91 79 78 97 40  7 13 43 87 74 34 31 84
       Card 113: 28 83 44 97 18 22 96 49 17 98 | 86 88 11 92  5 54 90 51 38 52 36 13 64 81 45 63 85 15 47 40 18 50 67 39 73
       Card 114: 89 28 69 44 47 58 20 60 25 92 | 97 61 98  4 37 30 68 96 86  6 50 34 71 53 78 57 10 36 35 40 67 33 93 17 75
       Card 115: 20 49 40 31  8 55 28 79 99 22 | 95 87 21 14 62 17 70 81 83 54 96 39 18 35 44 61 23 30 80 89 42 50  7 58 47
       Card 116:  3 75 52 97 60 24 50 13 54 71 | 17 21  1 98  8 79 59 88 97 53 82 28 34 45 93 69 13 85 38 26  7 37 43  5 22
       Card 117: 44 70 59 87 64 20 17 19 68 71 |  6 46 47 79 35 74 78 26 19 99 20 80 31  1 68  3 12 39 59 62 94 25 91 92 88
       Card 118: 65 74 73 48 97 21  1 43 57 29 | 73 25 58 67 56 41 19 20  1 12 99 48 47 96 68 50 88 49 80 10 39 75 11 82 79
       Card 119: 68 40  2 54 25 75 77 14 50 86 | 27 69 39 90 81 15 17 54 63 51 87 52 37 22  7 38 11  8 24 96 42 60  6 99  4
       Card 120: 43 63 34 38 57 39 23 29 16 41 | 90 93  1 72  9 80 94 73 28 65 87 25 78 33 98 66 52  2 36 30 92 54 47 24 12
       Card 121: 92 44 29 97 22 41 73 16 31 65 | 20 51 50 43 30 27 39 40 88 14 57 77 62 90 18 87 26 54 95 48 34 10 93 47  9
       Card 122: 61 12 44 52  4 69 42  1 77 54 | 24 23 11 39 78 52 12 69 73 42 47 43  4 29 37  1 36 77 19 61 95 54 94 44 28
       Card 123: 17 44 12 66 30 19 96 54 34 75 | 24 17 42 90 14 92 84 35 71 12 34 32 99 19 30 98 66 70 54 44 56 96 77 36 75
       Card 124: 99 74 53 87 15 93 58 96 24 44 | 15 58 96 29 49 50 87 93 86 53 12 51 24 44 60 74 19  8 67 99 76 72 45 80 81
       Card 125: 28 95  5 13 21 49 70 33 48  7 | 61 57  3 65 89 13  7 70  1 95 84 33 46 21 28 79  5  9 19 48  4 76 82 86 49
       Card 126:  1 83 21 50 35  3 55 84 38 81 | 61 80 30 26 83 84 38 92 73 21 42 14  1  6 81 15 50 57 55 56 35 63 67  3 93
       Card 127: 37  8 99 82 24  6 62 63 94 29 | 62  8 83 82 24 87 46 55 78 91 31 23 29 16 99 63 54  9 68 48 94  5 37  6 40
       Card 128: 18 47 91 78 74 63 49 42 85 10 |  4 10 82 20 97  3 71 74 62 95 60  9 28 73 47 32 14 18 49  7 83 92 85 53 78
       Card 129: 19  9 11 30 91 83 99 93 46 22 | 38 33 91 85 57 31 15 99 97 54 17 50 71  8  6 29 59 36 75 81 82 67 44 65 25
       Card 130: 62 13 98 39 78 28 63 38 41  6 | 51 99  1 98 81 40 39 63 62 52 25 47 55 13 34 41 38 18 14 73 97 28  6 48 78
       Card 131:  9 69 43 61 65 35  4 59 71 96 | 17  4 65 29 19 48  7 32 69 61 23 15  1 37  9 80 59 47 64 41 35 79 71 43 96
       Card 132: 40 62 73 49 19 51 30 95 92 55 | 77 79 31 51 92 41 49 27 13 40 99 52 68 98 32 26 29 20 60 25 65 72 11 70 19
       Card 133:  7 73  1 68 16 40 10 37 78 91 | 39  9 33 97 98 13 35 38 72 75 82 37 27 32 93 48 87 50 94  5 95 67 46 24 54
       Card 134: 43 78 49 90 73  7 38 95 12  4 | 10 79 30 12 25 39 32 68 95 78 45 40 73 37 70 49 41 46 91 42 23 18 76 15 90
       Card 135: 83 66 13 30 47 75 52 69 17 48 | 76  6  7  1 12 64 89 62 52 59 83 97 33 28 66 20 72 13 84 65 56 73 53 92 30
       Card 136: 63 90 52  1 43 85 83 22 47 57 | 17 44 77 36 79 20  1 65 29 50  6 81 78 76 70 97 90 68 67 34 31 82 88  2 25
       Card 137: 77 25 91 14 87 80  8 90 27 57 | 91 63 29 60 71 57 98 14 52 77 39 96 85 50 76 10  8 27 32  4 23  3 17 62 24
       Card 138: 33 17 89 10 85 90 79 72 13 53 | 30 45 81 89 47 57 32 80  1 82 84 97 16 90 66  4 75 94 29 54  9 78  3  8 93
       Card 139: 57 99 35 94  5 11 86 67 34 96 |  1 79 54 17 20 82 26 32  7 77 44 51 18  3 76 64  9 72 35 83 37  5 60 66 22
       Card 140:  6  9 44 98 64 92 30 14 62  4 | 43 49  9 79 42 27 20 69 36 24 47 19 14 38 98 54 77 73 41 56  4 15 86 10 44
       Card 141: 28 99 14 96 19 92 12 23 17 22 | 64 19 85 70 80 16 63 96  2  3 61 67 41  9 74 12 37 30 86 36 26 92 94 66 71
       Card 142: 28  2 68 35 71 10 98 57 79 14 | 44 78 23 24 59 49 21 47 75  6 60 40 22 15 46 41 71 53 87 90 32 52 35 72 37
       Card 143:  8 10 60 94 66 51 36 29 56 69 | 71 69  2  9 12 55  1 53 91 84 22 83 77  6 18 57 10 24 44 72 79 73 95 62 99
       Card 144: 21 12 42 22 50 10 88 18 19  9 | 25 72 96 47 60 85 23 10  7  3 26 81 59 80 38 91  5 70 95 97 53 33 67 74 92
       Card 145: 53  5 14 26 75 35 21 39 30 43 | 13 18  6 46 69 76 78 67 81 68 36 83 91 97 32 40 23 92 29 28 42 51 71 20 37
       Card 146: 18 66 74 11 31 91 15 55 95 87 | 54 74 55 18 89 28 95 72 62 38 15 92 91 31 44  9 81 87 88 66 24 23 71 16 11
       Card 147:  7 21 81 38 56  2 55 50 60 83 | 15  9 91 69 44 92 54 99 72 43 59 78 25 79 32 51 64 20 62 24 75 38 29 16 97
       Card 148: 46 52 47 60 29  9 67 22 10 23 | 92 82 54 91 74 55  1 73 26 61 64 90 28  2 72 40 27 32 51 20 89 31 75 97 99
       Card 149: 27 58 78 77 96 68 54 89 31 86 | 48 42 86  8 21 20 60 76 72 37  2 75 33 67 57 28 41 78  1 49 95 87 38 94 62
       Card 150: 38 19 90 28 21 59 10  7 66 80 | 68 60 40  3 50 55  6 65 64 59 47 20 56 70 14 29 26  4 22 74 17 69 46 75 87
       Card 151: 93 89 25 22 91 70 10 78 84 50 | 21 35 78 58 30 71 11 70 99 84 97 87 57 67 15  9 20 91 79 50 26 74 47 56 12
       Card 152: 68 34 95 27 26 87 56 85 92 78 | 58 33 87 81 64 95 27 48 85 72  6 62 65 50 39 96 37 92 28 80 60  1 83 31 74
       Card 153: 34 11 38 50 57 78 52 73  7  4 | 56 78  9 11 32 92 26 86 40 27 24 91 18 13  4 77 37 61 25 58 60  8 30 16 80
       Card 154: 40 88 81 92  6 33 60 25 98 37 | 77 28 68 92 73 41 81 24 40 67 11 44 17 91 82 39 69  8 57 62 29 76 99 47 79
       Card 155: 15 95 17 21 76 78 69 54  1 40 | 68 17 89 96 23 49 20 53 47 93 37 55 10 69 21  4  3  6 75 65 95 79 67 30 91
       Card 156: 82 50  7 62 47 55 85 26 41 17 | 71 94  8 43 83 90 76 20 66 21 35 85 61 53 18 37 16 14 99 46 62 47 64 57 33
       Card 157: 24 85  5 76 32 43 58 27 28 78 | 50 36 66 65  1  2 87 75 96 18 53 39  3 48 23 97 63 90  4 62 42 93 47 54 60
       Card 158: 92 31 19 15 16 21 50 20 17 59 | 56 32 60 65 42 46  3 82 89 96 28 41 45 51 23 17 40 95 62 93 61 70 35  2 67
       Card 159: 72 26 19 38 65 12 97 76  7 34 |  4  1 45 39 60 32 22  5 28 98 43 66  2  9 21 10 15 67 78 27 90 17 68 31 35
       Card 160: 60 24 21 33 74 59 10 82 22 87 | 26 96 29 12 55 38 71 76 25 68 78  6 50 13 51 99 65 17 98 42 73 54 44 37  9
       Card 161: 82  9 94 58 56 85 45 17 21 32 | 91  1 55 63 21 94 32 22 15 17 34 85 26  9 46  7  8  5 48 82 93 60 28 56 45
       Card 162: 28 45 14 62 60 73 92 49 11 95 | 73 44 28 42 22 98 80 86 74 51 14 18 62  8 92 96 45 13 65 49 95 39 60 11 85
       Card 163: 67 45 89 16 36  9 92 91 78 43 | 75 34 52 90 85 77 27  1 83 93 32 99 69  2 79 94 80 11 35 64 39 72 17 70 13
       Card 164: 95 30 90 87 96 81 16 91 66 13 | 97 12 72 54 67  3 61 27 62 68 19  7 65 50 35 39 73 53 18 94 59  4 71 64 83
       Card 165: 56 77  8 11 88 80 65  9 78 48 | 10 26 24 21 39  5 32 80 79 48 58  9 16 69  8 30 97 53 20 78  6 56  2 88 43
       Card 166: 21 57 49 50 24 79 44  7 84 40 | 50 16 11 93 36 66 46 24 59 35 99 21  5 56 89 57 92 41 82 37 40 12 79 20 44
       Card 167: 23  2 68 93 19 28 11 22  7 27 | 11 85 94 28 22 68  2 65 21 78 92 27 93 74 38 57 83 80  7 23 19 77 90 59 84
       Card 168: 93 39 75 95 97 10 83 35 66 62 |  7 79 76 91 39 83 75 93 56 89 37 10 15 72 35 84 14 26 97 71 57 95 62 66 44
       Card 169: 95 68 15 39 27 24 62 86 71 78 | 24 27 81 70  7 48 43 86 52 88 62 58 19 68 31  8 71 80 39 78 64 95 35 15  6
       Card 170: 63  4 96 68 10  3 37 44 70 78 | 13 58 97 36 57 68 78  8 79 96 70  1 64 14 63 51 99 37 41 49  7  3 72  4 71
       Card 171: 67 36 24 10  7 11 65 28  3 83 | 87 66 58 53 21 39 40 12  1 94 61 89 22 50 84 78 72 64 92 34 71 90 11 16 15
       Card 172: 30 72 95 64  5 51 61  6 39 94 | 31 37 30 71 91 75 10 74 81 36 14 19 73 42 82 97  6 47 51 64 72 45 53 80 60
       Card 173: 64 71 93 39 91 57 82 95 60 67 | 34 97 89 36 71 78 76  7  8 33 50 68 19 73 60 91 39 82 46 67 57 35 27 10 63
       Card 174: 70 68 35 62 73 42 26  3 86 51 |  3 10 69 84 60 58 27 44 41  9 22 49 89 54 86 70  1 50 83 42 37 74 85 78 71
       Card 175:  5 82 68 57 10 32 70 72 23 45 | 10 73 36 50 23 83 29 57 34 30 61 70 59 69 45 53 12 75 86 63  6 84 64 14 97
       Card 176: 35 27 24 75 70 12 29 78 17 91 | 64 57 75  5 81 79 98 99 65 18 87 66 68 12 19 74 55 80 50 10 78 56 36 61 41
       Card 177: 82  4 32 70 79  1 29 38 87 67 | 63 15 49 46 19  8 38 69 83 75 66 87 68 95 81 54 34 82  4 64 41 73 88 11 47
       Card 178: 27  6 50 58 37 87 96 56 25 85 | 96 27 64 60 15 54 12 47 30  5 78 61 90 37 40 93 87 23 53 85 70 34 16 50 63
       Card 179: 16 29 86 94 32 82 80 71 20 38 | 52  7  9 73 14 51 27 55 99 23 80 20 49 15  6 91 47 58 26 98 63 18 77 85 94
       Card 180: 87 33 35 88 32 23 13  4 52 92 | 45 78 30 19 54 17 56 31 88 86 18 69 16 12 96 82 60 13 62 81 67 46 72 80  4
       Card 181:  7 76 43 75 70 83 39 17 92 64 | 44 91 51 57 26 90 31 63 12 70 45 38  6 35 23 17 27 79 20 59  9 66 96 62 37
       Card 182: 42 10 44 12 46 55 60 83 94 18 | 21 78 38 96 64 92 11 48 98 30 63  1 75 58 31 29 67 79 43 91 34 25 39 99  6
       Card 183: 47 21 74 24 75 48 38 33 61 39 | 15  2 21 20 95 93 54 32 77 89 81  8 16 23 30 14 58 94 35 37 50 11 83 86  4
       Card 184: 68 41 47 24 74 25 38 12  6 58 | 46 48 20 31 37 40 59  5 73 52 21  7 57 55 86 17 49 80 10 22 81  4 13 91 26
       Card 185: 76 26 24 14 90 54 75 51 13 42 | 14 30 57 20 73 56 86 42 89 74 97 37 64 51 47 24 60 83 22 87 76 90 13 61 40
       Card 186:  2 63 80 91 41 12 85 88 34 30 | 81 29 19 64 52 36 41 18 42 54 44 43 40 63 76 23 98  4 65 38 24 30 62 99 87
       Card 187: 49 77 68 73 76 60 15 92 34 39 | 32 23 47 26 40 41 43 92 84 73 94 60 24 30 45  7 14 80 76 10  5  9 34 11 50
       Card 188: 11 96 50  5 37 58 42 81 79  3 | 29 78 77 30 92 89 18 88 15 95 91 40 60 14  8 62 28 90 33 32 98 63 21 36 46
       Card 189: 78  8 49 19  1 67 44 52 54 46 | 29 64 10 55  8 78 36 67 32 91 97 80 49  1 44 83 58 46 69 34 35 94  2 14 86
       Card 190:  4 86 43 89 60 80 18 67 11 87 | 19 13 60  4  6 18 11 56 89 26 43 65 85 41 22 86 87 91 71 28 67 80  2 74 47
       Card 191: 93  9 61 81 20 51 42 14 74 83 | 68 82 91 74 80 61 44 96 18 11  9 14 39 84 20 47 17 93 42 52 99 21 51 77 76
       Card 192: 41 83 44 12 68 56 79 70 55 34 | 90  4 34 20 51 83 74 32 49 41 86 36 66 42 29 85 37 33  1 24 10 97 40 27 12
       Card 193: 63 32 91 93 31  2 42 29 30  5 | 46 56 12 16 61 41 81 20 31 89 62 93  5 58  6 73 66 55 74 29 83 88 52 47 27
       Card 194: 34 54 16  3 62  6 67 40 57 53 | 91 63 31 59 52 56  6 33 84 55  3 72 40 14 54 35 43 67 97 64 53 44 28 37 81
       Card 195:  1 18 74 57 38 11 64 30  9 54 | 82 35 66 40 29 45 14 37 19 85 54 63 99 89  5 22 71 94 11 70 39 16 42 51  8
       Card 196: 95 44 35 80 30 64 82 60 75 47 | 88 45 15 26 90 34 93 85 43 66 77 11 17 65 39 33 56 23  3 20 35  5 44 47 36
       Card 197: 53 10 14 12 65 83 18 28 79 25 |  7 37 96 80 85 51 47 39 89 13 99 72  1 21 71 25 83 61 35 59  3 73 84 50 15
       Card 198: 29 97 30 81 41 34  9 47 21 39 |  8 88 94 67 16 68 37 82 85  3 36 32 41 90 89 40 46 74 29 81  4 93 76 55 87
       Card 199: 47 48 39 87 83 25 72 74 40 29 |  1 45 19 77 47 24 20 70 85 34 62 76 91 60 16 30 35 46 21 44  3 97 65  7  6
       Card 200: 43 45 85 27 99 88 52 35 28  3 | 18 54 53 96 80 62 49 15 90 14 44 48 36 33 60 79 63 17  5 13 82 24 10 20 64
       Card 201: 71 92 68 45 33 17 99 32 96 93 | 90 82 79 26 20 85 94 61 31 84 73 30  4 87 29 28 81 27 75 39 36 58 97 98 21"""
