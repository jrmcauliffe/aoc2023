module Day07Test exposing (suite)

import Day07 exposing (part1, part2)
import Expect exposing (equal)
import Test exposing (..)


suite : Test
suite =
    describe "Day 7 Tests"
        [ describe "Part 1"
            [ test "Given Example" <|
                \_ ->
                    example
                        |> part1
                        |> equal 6440
            , test "Given Problem" <|
                \_ ->
                    problem
                        |> part1
                        |> equal 253205868
            ]
        , describe "Part 2"
            [ test "Given Example" <|
                \_ ->
                    example
                        |> part2
                        |> equal 5905
            , test "Given Problem" <|
                \_ ->
                    problem
                        |> part2
                        |> equal 253907829
            ]
        ]


example =
    """32T3K 765
       T55J5 684
       KK677 28
       KTJJT 220
       QQQJA 483"""


problem =
    """32555 626
       4558J 55
       62K22 775
       T7JJT 530
       K4333 992
       4T884 682
       42222 694
       22333 122
       367Q4 661
       552A5 435
       2K2K2 131
       22Q2Q 141
       9J2TT 623
       T8JTJ 677
       A4999 432
       QQJ8Q 338
       K49QT 244
       TK8Q2 584
       3T386 597
       6J69J 930
       4J935 495
       QKT3A 420
       JJJ8J 365
       222J2 375
       JKKKJ 733
       8KK8K 314
       QJ533 119
       666JJ 725
       AA9J7 543
       3QQQ3 80
       QJ777 891
       4J2JT 487
       688KQ 819
       9J999 809
       43524 706
       KK9JK 901
       99Q9T 657
       823T4 71
       73222 226
       45K24 143
       Q3986 453
       TTA94 884
       A455A 57
       A7777 697
       8267J 606
       999TT 612
       QJQQQ 620
       K5QJ7 235
       9Q5TK 67
       8Q8Q8 690
       75567 688
       3Q3Q7 662
       KA959 2
       J8888 51
       JKT6A 187
       KK73K 419
       79999 255
       2222K 643
       J5T79 107
       25K43 105
       5KKK4 166
       T8J53 848
       AA5A5 561
       8885Q 483
       A67A7 260
       JT433 329
       45J66 106
       3A2Q4 669
       99T9A 525
       87Q58 11
       J85TT 615
       Q99TA 617
       TTTJT 514
       8Q888 616
       6Q2A6 998
       9Q83A 441
       Q88A9 820
       8JK7A 20
       J2A2K 189
       398KT 121
       TQ7KA 283
       TT3T8 120
       2JJ23 962
       A3TAA 723
       QK999 264
       3Q323 402
       T353A 724
       54QQQ 407
       Q447K 959
       7TTTT 622
       J4344 683
       8TA65 468
       72T6J 965
       38777 410
       J63A9 110
       5K9KJ 60
       3TJK7 769
       A3TTT 520
       576J5 744
       62622 770
       T9965 560
       J3888 342
       66566 207
       33J6J 658
       85727 45
       75747 670
       46832 90
       TK9KT 578
       T7K5Q 362
       KK46J 254
       QJA7T 532
       28JKK 162
       754TK 14
       55757 169
       4Q8A6 534
       26J22 326
       5Q9QQ 216
       222Q7 294
       34333 102
       8258A 703
       555KK 720
       38882 98
       23322 360
       74786 824
       KKK8K 197
       J7778 450
       49499 21
       JK98Q 215
       4A358 649
       9AAAT 18
       8J882 989
       63T8T 188
       7T77T 430
       5725A 427
       57697 341
       57257 991
       44799 262
       AA8AA 818
       53553 3
       7979Q 702
       8K482 728
       29K63 828
       J32J9 947
       T5ATQ 220
       J2772 504
       KTK2T 994
       JJQ3T 630
       33338 480
       KQTT4 666
       32A33 866
       AAAAQ 705
       QQ42Q 524
       72473 312
       85T45 869
       9689J 349
       K53A5 286
       2993T 749
       992T9 541
       KTTK8 738
       96963 97
       AA5AA 178
       55T55 668
       38K62 734
       7Q44Q 776
       3T6JT 333
       JA5T8 421
       99459 94
       446QQ 841
       KKK7A 850
       98696 871
       KKT74 236
       6J528 438
       T3TTT 840
       424JJ 35
       K8J8K 7
       KJJ37 711
       Q35J8 567
       2A233 750
       A2Q4A 861
       3T779 851
       A9AKA 648
       92JK3 261
       A4J78 746
       479TQ 650
       55558 229
       43444 135
       QQAQ4 767
       Q2QQT 793
       J2232 977
       6TTJ6 485
       KTKA9 62
       77A57 1
       9757Q 129
       343TT 502
       K4JK9 914
       53395 529
       9TT9K 117
       K2A8T 747
       A3328 214
       767K7 296
       J6A66 853
       5Q6K4 842
       J2QA4 356
       64J28 681
       528T4 352
       8AAAT 972
       2KA97 636
       QQQQA 33
       2A84A 390
       QK7KK 42
       75832 440
       8TJ8T 788
       22725 31
       94T33 754
       49T29 588
       Q6Q55 73
       5T455 716
       T33J3 580
       5555J 388
       J5J22 306
       K3T9A 191
       55259 109
       J8T88 290
       85998 234
       J979J 583
       Q6739 637
       TK689 325
       85K5K 433
       5T45T 552
       J3636 155
       23623 673
       8T43K 685
       3Q3Q3 539
       Q7KAQ 642
       92JK9 172
       6666A 644
       2JQTQ 345
       88J86 241
       5JJAA 971
       8368J 790
       A8Q45 704
       TQ985 659
       2626J 517
       QQ3QQ 206
       A726A 507
       Q5A49 344
       44686 153
       795T8 816
       9T7Q6 13
       49448 209
       7J773 346
       TTK55 357
       64T94 736
       3A3AA 805
       53TJ3 228
       Q2999 452
       T22TT 412
       57777 921
       77737 66
       34437 559
       J5852 137
       Q8T8T 586
       6KJK6 880
       99994 331
       KKTTK 618
       628T7 935
       53792 386
       T776T 920
       88JJ8 147
       6Q96T 864
       222JJ 984
       89889 562
       AJKA9 221
       87A54 6
       QQ4Q4 765
       2QQQ2 904
       QQ3Q6 980
       KK4KK 339
       AA3AQ 899
       QQ4K6 878
       J4884 516
       4J232 379
       8786J 967
       8QTQT 478
       T7339 985
       2J442 482
       87444 508
       34443 151
       K8TQ7 233
       J6866 587
       3Q333 425
       QA88A 784
       KK4A7 679
       AAAKA 766
       7575K 595
       TTTTQ 868
       2AA22 125
       558T5 150
       TAJ8T 321
       665QQ 79
       8QTJ7 472
       A4555 22
       45345 491
       773A7 575
       J6T32 687
       9AAAA 813
       QKJTA 569
       K2TT3 897
       TTJT6 239
       56656 269
       55525 951
       339J3 745
       A7K62 845
       A6AAA 134
       8Q833 932
       A8A5A 185
       J233K 373
       TTK22 966
       4K884 303
       Q53QA 566
       J62J8 761
       9697Q 846
       79963 825
       6A53J 132
       KKK85 737
       TT7J7 167
       QKAAQ 10
       JQQAQ 464
       49624 950
       JK999 126
       7Q77Q 398
       T2JT5 934
       77J8J 127
       893TJ 610
       45994 772
       26656 808
       5333J 86
       K4J44 900
       58545 519
       5QQQ5 451
       TJJ22 50
       5JK68 763
       9KK74 505
       KQKJK 270
       67T66 52
       22226 56
       Q6Q45 603
       AA887 699
       A3249 237
       99455 912
       74974 814
       QQ4QQ 9
       J2535 351
       K7J75 640
       4577A 100
       5Q92T 212
       A6AAJ 639
       QA9AA 676
       7JK5J 996
       6QKK6 198
       792T6 242
       7AJ47 479
       JA787 854
       KKJKK 399
       94667 173
       A3AAA 624
       38A57 581
       KKKK5 47
       7QJ29 136
       K3333 232
       Q6T2K 671
       33A35 634
       AAK8K 544
       65792 481
       AKQT5 830
       A797T 292
       J3QQ4 324
       QKT65 510
       333J3 833
       52A2A 742
       KKKAK 952
       J5A64 651
       99925 183
       8A888 961
       A4Q5K 113
       2AJT9 515
       24424 811
       988J8 740
       T545Q 28
       AQ9K6 942
       93993 812
       KKT43 265
       JA494 322
       44TT5 332
       36TQJ 625
       45QTA 177
       9K9KK 406
       K6K5J 156
       46464 607
       763J3 509
       J9A4A 84
       33373 849
       6QJQQ 192
       55553 913
       22J72 789
       AKK5Q 512
       T3T33 865
       55QQ5 250
       TJK6T 533
       QTQK2 986
       87234 460
       7A772 499
       64666 165
       97777 528
       99Q9Q 874
       32A26 29
       3T33Q 258
       K4947 1000
       J73T8 318
       J9622 247
       TTTT8 909
       62662 768
       KAAAK 393
       K9555 573
       367A5 225
       33J34 847
       75898 954
       7KTJ5 91
       KTKQQ 473
       22T2T 783
       9J5A4 759
       57888 320
       647QQ 922
       2J798 963
       64K42 836
       49KK9 563
       842K4 12
       39793 551
       A4968 970
       22J2T 604
       33389 955
       75975 713
       K4KAT 905
       87TJ8 867
       97645 894
       6JQ9T 291
       4KK3K 308
       KTTJT 83
       98595 288
       3J5J3 918
       88K88 741
       5T7K9 678
       3AAJ3 656
       3QJ3J 523
       J754T 317
       5796K 732
       J3767 608
       723QJ 700
       K572T 335
       TTTQ5 859
       42J2A 781
       38666 358
       TATAT 334
       T7AAA 572
       AA793 227
       A7J88 708
       QJQQJ 448
       4K4AJ 540
       J5A5A 469
       243TT 27
       88T88 149
       T222K 739
       T463A 44
       6TTTT 911
       43QQQ 590
       94AAQ 834
       7Q7JJ 196
       83447 5
       99K8K 310
       6A42J 353
       29939 267
       78882 77
       QTT7T 557
       T3QTT 140
       3A3A3 794
       7A7AA 367
       899J9 542
       76822 810
       53345 602
       J6K7J 41
       TJ53A 442
       99977 821
       4J77T 138
       285TQ 488
       888KK 889
       5K255 513
       3T329 879
       J6Q3Q 101
       TQ457 37
       64898 337
       6A26A 78
       4J444 470
       TT555 461
       9K999 40
       TTAA2 758
       67J63 240
       8TTT7 64
       K6K74 748
       K2K2J 936
       82828 274
       A2TK7 890
       JJJ22 600
       4AJ3K 815
       299JJ 613
       Q7AJ9 359
       J4484 465
       ATTTT 152
       6385T 463
       65TK6 416
       654JT 200
       T4TTJ 553
       994J4 527
       TTJ4K 925
       7Q444 838
       3474J 547
       AAAAT 535
       339Q3 276
       37737 148
       7935A 837
       27A27 36
       77778 302
       KKK3K 459
       63388 284
       AAQQQ 680
       96A52 364
       JJJJJ 735
       7K523 752
       92K9K 799
       Q4346 391
       A6T2Q 298
       ATK99 281
       92T37 128
       39A93 593
       7TJTT 785
       89888 497
       8793J 518
       KK444 293
       TTQ65 176
       KKA3K 266
       TJ8A3 158
       787QK 195
       37332 903
       QQ89Q 719
       22232 614
       88284 190
       Q332Q 764
       K2252 795
       6QAA6 621
       69939 397
       JK42K 489
       J7KKK 978
       T5KTT 717
       555Q5 48
       88877 361
       62A9K 252
       79236 30
       6664Q 38
       43388 394
       95863 238
       QQAJA 522
       27KQ4 886
       8QKQA 411
       99A9A 946
       22236 164
       2Q6TQ 205
       228A2 46
       492J4 976
       TQJ53 49
       J6JA6 664
       K8A8A 492
       8T47K 454
       4AAKA 997
       8K596 248
       497QJ 787
       J4JA8 832
       T5687 888
       6T656 938
       3QK69 803
       73QT8 872
       A56K9 287
       8AA99 910
       JJ559 213
       JJQ34 347
       46462 852
       Q3Q8T 827
       Q838T 271
       KKAK6 133
       95953 413
       7T75J 663
       J69J2 437
       K9955 230
       K82KK 710
       KQTQ4 490
       994TJ 455
       58AJ2 186
       AA7A4 179
       QKK4K 124
       7658A 251
       5JQ52 987
       95Q6T 170
       KJ8J8 960
       78867 598
       KJ469 801
       9TTTT 15
       83A3Q 773
       9J496 555
       ATAAT 381
       525J2 201
       95777 674
       J97TK 59
       44454 712
       K333J 104
       3555J 953
       AJ32J 383
       4TK26 449
       384J6 692
       K7KKK 718
       AT3T9 157
       82J22 974
       2TTTT 23
       5Q7Q5 82
       68666 876
       4ATA4 596
       T9J99 343
       88A8K 937
       TT7T7 782
       Q4244 184
       385K7 693
       JJK53 993
       4K99A 61
       47467 654
       J6677 467
       5J443 313
       246AQ 69
       5A29J 594
       86586 401
       J2TJT 180
       44988 130
       5JJ52 571
       646J8 300
       4Q22Q 531
       4TQJQ 902
       68885 256
       5AA65 210
       9999Q 92
       KKKTK 395
       JT4A6 988
       5544K 295
       9T65J 444
       33743 305
       884T8 387
       93233 494
       96999 431
       K9J8J 990
       T7T54 249
       57555 503
       QK6K3 181
       8ATT8 751
       99J22 426
       AQ9QQ 548
       KAAKK 638
       A242K 791
       66755 870
       T4543 219
       Q4Q32 445
       8K492 585
       3J947 823
       Q42QA 24
       99399 369
       K55J8 755
       7J6J6 54
       AAK5A 817
       Q7QQQ 511
       67QQ3 896
       38TJ4 146
       62493 223
       TAAQT 929
       ATAT7 400
       43348 16
       QK495 589
       94T9Q 112
       5J55J 895
       45595 873
       J9JAJ 860
       A55QA 19
       T483T 641
       57T32 434
       264KA 475
       T3T55 304
       66J4J 779
       K6AQA 804
       TQQ5T 968
       77AAJ 259
       25258 627
       4AT6K 85
       79795 780
       J3364 632
       23KAT 418
       4QQTQ 4
       8KJ42 665
       Q4739 564
       75J2K 123
       29Q85 982
       2J5A2 116
       7Q777 979
       29J99 217
       7628A 72
       A84AK 297
       J4745 727
       K8TJ9 204
       QT246 199
       QQQJ7 486
       K2KKK 549
       QTTJ5 493
       JTJTT 257
       77K7K 160
       QQ44A 202
       Q7Q78 975
       27772 63
       72776 316
       2KK52 118
       37797 926
       7AQ25 949
       77J6Q 709
       KT539 753
       8QJQJ 368
       998J5 380
       7333J 372
       J8469 81
       7777J 299
       82773 698
       8Q8J5 655
       27TA5 74
       J2T2T 577
       T4444 506
       56A34 70
       52T52 378
       JT327 778
       622K6 280
       935Q6 919
       AA4A4 944
       22ATT 68
       5JQKQ 43
       98T68 862
       727JJ 65
       9KQ4K 958
       AJAA9 115
       J333Q 392
       J39J3 374
       TK33K 885
       49T38 500
       QA884 103
       9J9J4 633
       8AA86 163
       99995 645
       3K499 389
       33T33 999
       A4AAA 792
       3TQA8 945
       77776 915
       3A92Q 8
       J9K8K 53
       62666 653
       A3766 498
       9434J 301
       5ATAT 829
       56K28 882
       222Q2 285
       25K63 382
       AAQQA 646
       JK9JA 689
       K7K33 34
       78KKK 340
       J2QQ2 203
       84888 422
       84J46 757
       68878 599
       98989 777
       42K23 208
       67667 863
       33988 582
       52T8T 957
       T44Q4 981
       54632 429
       69332 628
       5JAA2 611
       4A4A4 652
       K3535 111
       5J4J5 786
       87JJ4 171
       K5KA5 969
       876K4 144
       KQKTT 396
       93839 743
       36K8K 175
       6J256 907
       AA9K6 417
       A4A47 892
       8T99T 537
       88788 328
       Q5QQQ 797
       ATJAQ 95
       999JJ 424
       Q3J64 931
       T4T4T 526
       TTJ38 336
       5K72A 730
       K77QK 218
       K44K2 917
       64AAA 58
       66ATT 591
       AQA8K 576
       TT75K 423
       68J7T 721
       T8K8K 96
       777JJ 771
       QJ5Q5 635
       4966K 275
       JT836 898
       4TTTA 99
       QTTQQ 282
       55656 943
       22454 458
       99868 883
       J55AT 501
       K8992 279
       2244T 558
       66667 174
       49943 246
       328T8 385
       995JA 311
       AQ5J9 629
       A8AA8 906
       J5K65 796
       62QKK 835
       3A35A 93
       66T66 222
       3J633 414
       6JKK2 731
       8AAJK 538
       5J335 354
       29932 550
       K5QQ5 278
       83383 928
       A4T9Q 684
       AAJAA 88
       6T22A 142
       22228 428
       6234Q 268
       639K5 476
       T622T 887
       68662 471
       K36A9 715
       52245 858
       J444J 462
       A2J9K 701
       4J74K 194
       6KKK9 330
       JJK38 277
       ATAKT 17
       7896Q 404
       8K7K7 243
       43324 802
       4485Q 844
       4K9Q4 760
       342J3 983
       J888A 881
       A8KQJ 660
       QQ4Q7 756
       AAAA2 807
       AT368 75
       6TKTT 521
       8KJ29 253
       94JT5 822
       53335 108
       4AQJQ 139
       K4K34 916
       K44JK 370
       A3333 327
       K938A 415
       88AA3 466
       3QKJ9 87
       7TJQT 855
       67763 26
       325Q8 371
       K2KK2 826
       22QQ8 263
       4Q33Q 798
       6358K 231
       A6AA6 806
       22J78 315
       J2845 211
       K36AQ 408
       84Q48 319
       89878 956
       K69J2 403
       564KA 436
       49292 154
       7629A 939
       J4999 631
       3T7T7 574
       6T3T6 893
       555JK 579
       J7538 592
       A5A58 875
       J7898 456
       5JT5T 457
       45JK3 159
       6T3T9 536
       87778 774
       433J4 877
       8T599 570
       QJ632 89
       4J222 477
       TTK8T 839
       96996 601
       3KJ3K 446
       943T7 545
       KK3JQ 973
       A37T2 25
       J6666 355
       Q4775 908
       66KK2 726
       93956 933
       5JJ35 667
       44994 474
       T8J69 161
       TJ5Q2 289
       4Q44Q 447
       Q7A69 647
       5T5TT 273
       J5KKQ 484
       24422 409
       TQK76 377
       KKK66 800
       QKKQA 384
       KKKA4 565
       J779J 691
       76566 307
       2J2Q6 941
       8A568 309
       94AA5 856
       J247J 696
       99QQJ 995
       43339 964
       5AJ6K 729
       J67KQ 376
       4454Q 672
       KT6A7 843
       QT7Q8 443
       TQQ4T 363
       54548 323
       3K8QK 940
       34777 76
       327T8 405
       92929 927
       27793 546
       9T27Q 924
       QKJ8K 32
       52225 923
       3T242 366
       86A46 568
       J9796 39
       77A7A 145
       KKKT6 722
       TQQQQ 182
       JQJJQ 948
       54622 556
       92889 224
       44666 193
       TAATJ 686
       22292 619
       K2853 714
       A9A9A 857
       38783 348
       35874 439
       29TJQ 605
       A5992 695
       4TTT9 168
       77474 272
       3ATTJ 245
       37TA6 496
       A5A55 707
       564A8 554
       48J5Q 831
       28K28 350
       84484 762
       3A337 114
       A33J3 675
       5T694 609"""
