import Test.Tasty
import Test.Tasty.HUnit
import MiniK
import Rewrite
import qualified NormalizedMap
import qualified Languages.Imp as Imp
import Debug.Trace

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite =
    testGroup "Test suite" [impTests]

impTests :: TestTree
impTests =
    testGroup "Imp tests" [assignTests, ifTests, whileTests, lawsTests
        , loadTests]

assignTests :: TestTree
assignTests =
    testGroup
        "Assign tests"
        [ singleAssign
        , multipleAssign
        ]

ifTests :: TestTree
ifTests =
    testGroup
        "If tests"
        [ ifTrueTest
        , ifFalseTest
        , ifLookupTest
        ]

whileTests :: TestTree
whileTests =
    testGroup
        "While tests"
        [ incrementTo10
        , checkIsPrime
        ]

lawsTests :: TestTree
lawsTests =
    testGroup
        "Laws tests"
        [ mapCommutativity ]

loadTests :: TestTree
loadTests =
    testGroup
        "Load tests"
        [ loadTest ]

{-
    assign(a, 1)
-}
singleAssign :: TestTree
singleAssign =
    testCase "Single assignment" $ do
        let konfig =
                Konfiguration
                    { k = KSeq assignA1 KEmpty
                    , kState = MapEmpty
                    }
            expectedResult =
                Konfiguration
                    { k = KEmpty
                    , kState =
                        MapCons (Id "a") (I 1) MapEmpty
                    }
            actualResult =
                rewrite konfig Imp.rewriteRules
        assertEqual "" expectedResult actualResult

{-
    assign(a, 1)
    assign(b, 2)
    assign(c, 3)
-}
multipleAssign :: TestTree
multipleAssign =
    testCase "Multiple assignments" $ do
        let konfig =
                Konfiguration
                    { k = KSeq assignA1 (KSeq assignB2 (KSeq assignC3 KEmpty))
                    , kState = MapEmpty
                    }
            expectedResult =
                Konfiguration
                    { k = KEmpty
                    , kState =
                        MapCons
                            (Id "a")
                            (I 1)
                            (MapCons
                                (Id "b")
                                (I 2)
                                (MapCons
                                    (Id "c")
                                    (I 3)
                                    MapEmpty
                                )
                            )
                    }
            actualResult = rewrite konfig Imp.rewriteRules
        assertEqual "" expectedResult actualResult

{-
    if(0 < 1 && 6%2 < 1)
        then assign(a, 1)
        else assign(b, 2)
-}
ifTrueTest :: TestTree
ifTrueTest =
    testCase "Simple if true" $ do
        let trueBranch = assignA1
            falseBranch = assignB2
            konfig =
                Konfiguration
                    { k =
                        KSeq
                            (KSymbol
                                "if"
                                [ simpleIfCondition
                                , trueBranch
                                , falseBranch
                                ]
                            )
                            KEmpty
                    , kState = MapEmpty
                    }
            expectedResult =
                Konfiguration
                    { k = KEmpty
                    , kState =
                        MapCons (Id "a") (I 1) MapEmpty
                    }
            actualResult = rewrite konfig Imp.rewriteRules
        assertEqual "" expectedResult actualResult

{-
    if(not(0 < 1 && 6%2 < 1))
        then assign(a, 1)
        else assign(b, 2)
-}
ifFalseTest :: TestTree
ifFalseTest =
    testCase "Simple if false" $ do
        let trueBranch = assignA1
            falseBranch = assignB2
            konfig =
                Konfiguration
                    { k =
                        KSeq
                            (KSymbol
                                "if"
                                [ simpleNegatedIfCondition
                                , trueBranch
                                , falseBranch
                                ]
                            )
                            KEmpty
                    , kState = MapEmpty
                    }
            expectedResult =
                Konfiguration
                    { k = KEmpty
                    , kState =
                        MapCons (Id "b") (I 2) MapEmpty
                    }
            actualResult = rewrite konfig Imp.rewriteRules
        assertEqual "" expectedResult actualResult

{-
    if(a < 1 && 6%2 < 1)
        then assign(a, 1)
        else assign(b, 2)
-}
ifLookupTest :: TestTree
ifLookupTest =
    testCase "If condition does lookup" $ do
        let trueBranch = assignA1
            falseBranch = assignB2
            konfig =
                Konfiguration
                    { k =
                        KSeq
                            (KSymbol
                                "if"
                                [ withLookupIfCondition
                                , trueBranch
                                , falseBranch
                                ]
                            )
                            KEmpty
                    , kState =
                        MapCons (Id "a") (I 0) MapEmpty
                    }
            expectedResult =
                Konfiguration
                    { k = KEmpty
                    , kState =
                        MapCons (Id "a") (I 1) MapEmpty
                    }
            actualResult = rewrite konfig Imp.rewriteRules
        assertEqual "" expectedResult actualResult

{-
    assign(a, 1)
    while(a < 11)
        assign(a, a + 1)
-}
incrementTo10 :: TestTree
incrementTo10 =
    testCase "Program which increments a counter to 10" $ do
        let konfig =
                Konfiguration
                    { k =
                        KSeq
                            assignA1
                            ( KSeq
                                (KSymbol
                                    "while"
                                    [ aLessThan 10
                                    , incrementA
                                    ]
                                )
                                KEmpty
                            )
                    , kState = MapEmpty
                    }
            expectedResult =
                Konfiguration
                    { k = KEmpty
                    , kState =
                        MapCons (Id "a") (I 10) MapEmpty
                    }
            actualResult = rewrite konfig Imp.rewriteRules
        assertEqual "" expectedResult actualResult

{-
    assign(isPrime, 0)
    assign(counter, 2)
    while(counter < n && isPrime < 1)
        if(n % counter < 1)
            then
                assign(isPrime, 1)
            else .K
        assign(i, i + 1)
-}
checkIsPrime :: TestTree
checkIsPrime =
    testCase "Program which checks if a number is prime" $ do
        let actualResult1 = rewrite (program 7) Imp.rewriteRules
            actualResult2 = rewrite (program 10) Imp.rewriteRules

        assertBool "Program is not stuck" (not (canBeRewritten actualResult1))
        assertBool "Program is not stuck" (not (canBeRewritten actualResult2))
        assertBool "Correct value of isPrime" (not (flagIndicatesIsPrime actualResult1))
        assertBool "Correct value of isPrime" (flagIndicatesIsPrime actualResult2)
  where
    program inputNumber =
        Konfiguration
            { k =
                KSeq
                    initIsPrime
                    (KSeq
                        initCounter
                        (KSeq
                            (KSymbol
                                "while"
                                [ whileCondition inputNumber
                                , KSeq
                                    (KSymbol
                                        "if"
                                        [ ifCondition inputNumber
                                        , assignIsPrimeTrue
                                        , KEmpty
                                        ]
                                    )
                                    (KSeq
                                        incrementCounter
                                        KEmpty
                                    )
                                ]
                            )
                            KEmpty
                        )
                    )
            , kState = MapEmpty
            }
    flagIndicatesIsPrime Konfiguration { kState } =
        let flag =
                NormalizedMap.lookupConcreteId "isPrime"
                . NormalizedMap.normalize
                $ kState
         in
            case flag of
                Just value ->
                    case value of
                        I 0 -> False
                        I 1 -> True
                        _ -> error "Flag should be either 0 or 1."
                _ -> error "Flag is missing from configuration state."

{-
    assign(c, 3); state: {d: 4, a: 1, b: 2}
-}
mapCommutativity :: TestTree
mapCommutativity =
    testCase "Add element to exist state" $ do
        let konfig =
                Konfiguration
                    { k =
                        KSeq
                            assignC3
                            KEmpty
                    , kState = MapCons (Id "d") (I 4)
                        . MapCons (Id "a") (I 1)
                        $ MapCons (Id "b") (I 2) MapEmpty
                    }
            expectedResult =
                Konfiguration
                    { k = KEmpty
                    , kState = MapCons (Id "a") (I 1)
                        . MapCons (Id "b") (I 2)
                        . MapCons (Id "c") (I 3)
                        $ MapCons (Id "d") (I 4) MapEmpty
                    }
            actualResult = rewrite konfig Imp.rewriteRules
        assertEqual "" expectedResult actualResult

{-
    assign(a1, 1)
    while(a1 < 2)
        assign(a1, a1 + 1)
        assign(a1, a1)
        assign(a2, 1)
        while(a2 < 4)
            assign(a2, a2 + 1)
            assign(a2, a1 + a2)
                assign(a3, 1)
                while(a3 < 6)
                    ...
-}
loadTest :: TestTree
loadTest =
    testCase "Long nested while loop" $ do
        let konfig =
                Konfiguration
                    { k =
                        KSeq
                            (whileBody depth)
                            KEmpty
                    , kState = MapEmpty
                    }
            depth = 62
            actualResult =
                traceEvent "Test: long nested while loop - begin" $
                rewrite konfig Imp.rewriteRules
            counterVal = NormalizedMap.lookupConcreteId (counterId depth)
                . NormalizedMap.normalize $ kState actualResult
            expectedResult = Just $ I 4611686018427387904
        assertEqual "" expectedResult counterVal
        traceEvent "Test: long nested while loop - end" $ pure ()
  where
    counterId :: Int -> String
    counterId = ("a" <>) . show
    whileBody :: Int -> MiniK
    whileBody depth = whileBody' 1
      where
      whileBody' n = let counter = IntId . Id $ counterId n in
          (KSeq
              (KSymbol
                  "assign"
                  [ KInt counter
                  , KInt $ I 0
                  ])
              (KSymbol
                  "while"
                  [ KBool $ LT' counter (I $ n * 2)
                  , KSeq
                      (KSymbol
                          "assign"
                          [ KInt counter
                          , KInt (Plus counter (I 1))
                          ])
                      (KSeq
                          (KSymbol
                              "assign"
                              [ KInt counter
                              , KInt $ foldl Plus counter
                                  $ IntId . Id . counterId
                                  <$> take n [1..]
                              ])
                          if n < depth then whileBody' $ n + 1 else KEmpty)
                  ]))

assignA1, assignB2, assignC3 :: MiniK
assignA1 =
    KSymbol
        "assign"
        [ KInt (IntId (Id "a"))
        , KInt (I 1)
        ]
assignB2 =
       KSymbol
           "assign"
           [ KInt (IntId (Id "b"))
           , KInt (I 2)
           ]
assignC3 =
       KSymbol
           "assign"
           [ KInt (IntId (Id "c"))
           , KInt (I 3)
           ]

simpleIfCondition, simpleNegatedIfCondition :: MiniK
simpleIfCondition =
    KBool
        (And
            (LT' (I 0) (I 1))
            (LT' (Mod (I 6) (I 2)) (I 1))
        )
simpleNegatedIfCondition =
    KBool
        (Not
            (And
                (LT' (I 0) (I 1))
                (LT' (Mod (I 6) (I 2)) (I 1))
            )
        )

withLookupIfCondition :: MiniK
withLookupIfCondition =
    KBool
        (And
            (LT' (IntId (Id "a")) (I 1))
            (LT' (Mod (I 6) (I 2)) (I 1))
        )

aLessThan :: Int -> MiniK
aLessThan n =
    KBool (LT' (IntId (Id "a")) (I n))

incrementA :: MiniK
incrementA =
    KSymbol
        "assign"
        [ KInt (IntId (Id "a"))
        , KInt (Plus (IntId (Id "a")) (I 1))
        ]

initIsPrime :: MiniK
initIsPrime =
    KSymbol
        "assign"
        [ KInt (IntId (Id "isPrime"))
        , KInt (I 0)
        ]

initCounter :: MiniK
initCounter =
    KSymbol
        "assign"
        [ KInt (IntId (Id "counter"))
        , KInt (I 2)
        ]

whileCondition :: Int -> MiniK
whileCondition inputNumber =
    KBool (And counterLessThanN isPrimeLessThan1)
  where
    counterLessThanN =
        LT' (IntId (Id "counter")) (I inputNumber)
    isPrimeLessThan1 =
        LT' (IntId (Id "isPrime")) (I 1)

ifCondition :: Int -> MiniK
ifCondition inputNumber =
    KBool (LT' (Mod (I inputNumber) (IntId (Id "counter"))) (I 1))

assignIsPrimeTrue :: MiniK
assignIsPrimeTrue =
    KSymbol
        "assign"
        [ KInt (IntId (Id "isPrime"))
        , KInt (I 1)
        ]

incrementCounter :: MiniK
incrementCounter =
    KSymbol
        "assign"
        [ KInt (IntId (Id "counter"))
        , KInt (Plus (IntId (Id "counter")) (I 1))
        ]
