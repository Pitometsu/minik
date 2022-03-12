module Languages.Imp where

import MiniK

{-
    rule
        <k> assign(X:Id, I:Int) ~> Rest:K => Rest </k>
        <kState> M:Map => X |-> I M </kState>
-}
assign :: RewriteRule
assign =
    RewriteRule
        { left =
            Konfiguration
                { k = K $
                    KSymbol
                        "assign"
                        [ KInt . K . Ref $ KVar @OfIdType "X"
                        , KInt $ KVar @OfIntType "I"
                        ]
                        $ KVar @(OfMiniK Value) "Rest"
                , kState = KVar @(OfMapType Value) "M"
                }
        , right =
            Konfiguration
                { k = KVal $ KVar @(OfMiniK Redex) "Rest"
                , kState = K $
                    MapCons
                        (KVar @OfIdType "X")
                        (KVar @OfIntType "I")
                        $ KVar @(OfMapType Value) "M"
                }
        , sideCondition = K $ B True
        }

{-
    rule
        <k> ( if(B:Bool)
                then TrueBranch:K
                else FalseBranch:K
            ) ~> Rest:K
            => TrueBranch ~> Rest
        </k>
        <kState> M:Map => M:Map </kState>
      requires B
-}
ifTrue :: RewriteRule
ifTrue =
    RewriteRule
        { left =
            Konfiguration
                { k = K $
                    KSymbol
                        "if"
                        [ KBool $ KVar @OfBoolType "B"
                        , KExp $ KVar @(OfMiniK Value) "TrueBranch"
                        , KExp $ KVar @(OfMiniK Value) "FalseBranch"
                        ]
                        $ KVar @(OfMiniK Value) "Rest"
                , kState = KVar @(OfMapType Value) "M"
                }
        , right =
            Konfiguration
                { k = KSeq (KVar @(OfMiniK Redex) "TrueBranch")
                      $ KVar @(OfMiniK Redex) "Rest"
                , kState = KVar @(OfMapType Value) "M"
                }
        , sideCondition = KVar "B"
        }

{-
    rule
        <k>
            ( if(B:Bool)
                then TrueBranch:K
                else FalseBranch:K
            ) ~> Rest:K
            => FalseBranch ~> Rest
        </k>
        <kState> M:Map => M:Map </kState>
      requires not(B)
-}
ifFalse :: RewriteRule
ifFalse =
    RewriteRule
        { left =
            Konfiguration
                { k = K $
                    KSymbol
                        "if"
                        [ KBool $ KVar "B"
                        , KExp $ KVar "TrueBranch"
                        , KExp $ KVar "FalseBranch"
                        ]
                        $ KVar "Rest"
                , kState = KVar "M"
                }
        , right =
            Konfiguration
                { k = KSeq (KVar "FalseBranch")
                      $ KVar "Rest"
                , kState = KVar "M"
                }
        , sideCondition = K . Not $ KVar "B"
        }
{-
    rule
        <k> ( while(B:Bool) WhileBody:K ) ~> Rest:K
        =>
            ( if(B)
                then WhileBody ~> ( while(B) WhileBody )
                else .K
            ) ~> Rest
        </k>
        <kState> M:Map => M:Map </kState>
-}
while :: RewriteRule
while =
    RewriteRule
        { left =
            Konfiguration
                { k = K $
                    KSymbol
                        "while"
                        [ KBool $ KVar "B"
                        , KExp $ KVar "WhileBody"
                        ]
                        $ KVar "Rest"
                , kState = KVar "M"
                }
        , right =
            Konfiguration
                { k = KVal . K $
                    KSymbol
                        "if"
                        [ KBool $ KVar "B"
                        , KExp $ KSeq
                            (KVar "WhileBody")
                            (K $ KSymbol
                                "while"
                                [ KBool $ KVar "B"
                                , KExp . KVal $ KVar "WhileBody"
                                ]
                                . KVal $ K KEmpty)
                        , KExp . KVal $ K KEmpty
                        ]
                        . KVal $ KVar "Rest"
                , kState = KVar "M"
                }
        , sideCondition = K $ B True
        }

rewriteRules :: [RewriteRule]
rewriteRules =
    [ assign
    , ifTrue
    , ifFalse
    , while
    ]
