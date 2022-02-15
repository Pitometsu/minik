import Test.Tasty
import Test.Tasty.HUnit
import MiniK
import Rewrite
import qualified NormalizedMap
import qualified Languages.Imp as Imp

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite =
    testGroup "Test suite" [impTests]

impTests :: TestTree
impTests =
    testGroup "Imp tests" [assignTests, ifTests, whileTests]

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
                                    [ aLessThan11
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

aLessThan11 :: MiniK
aLessThan11 =
    KBool (LT' (IntId (Id "a")) (I 11))

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
