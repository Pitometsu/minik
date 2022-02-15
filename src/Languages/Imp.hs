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
                { k =
                    KSeq
                        (KSymbol
                            "assign"
                            [ KInt (IntId (IdVar "X"))
                            , KInt (IntVar "I")
                            ]
                        )
                        (KVar "Rest")
                , kState = MapVar "M"
                }
        , right =
            Konfiguration
                { k = KVar "Rest"
                , kState =
                    MapCons
                        (IdVar "X")
                        (IntVar "I")
                        (MapVar "M")
                }
        , sideCondition = B True
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
                { k =
                    KSeq
                        (KSymbol
                            "if"
                            [ KBool (BoolVar "B")
                            , KVar "TrueBranch"
                            , KVar "FalseBranch"
                            ]
                        )
                        (KVar "Rest")
                , kState = MapVar "M"
                }
        , right =
            Konfiguration
                { k = KSeq (KVar "TrueBranch") (KVar "Rest")
                , kState = MapVar "M"
                }
        , sideCondition = BoolVar "B"
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
                { k =
                    KSeq
                        (KSymbol
                            "if"
                            [ KBool (BoolVar "B")
                            , KVar "TrueBranch"
                            , KVar "FalseBranch"
                            ]
                        )
                        (KVar "Rest")
                , kState = MapVar "M"
                }
        , right =
            Konfiguration
                { k = KSeq (KVar "FalseBranch") (KVar "Rest")
                , kState = MapVar "M"
                }
        , sideCondition = Not (BoolVar "B")
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
                { k =
                    KSeq
                        (KSymbol
                            "while"
                            [ KBool (BoolVar "B")
                            , KVar "WhileBody"
                            ]
                        )
                        (KVar "Rest")
                , kState = MapVar "M"
                }
        , right =
            Konfiguration
                { k =
                    KSeq
                        (KSymbol
                            "if"
                            [ KBool (BoolVar "B")
                            , KSeq
                                (KVar "WhileBody")
                                (KSeq
                                    (KSymbol
                                        "while"
                                        [ KBool (BoolVar "B")
                                        , KVar "WhileBody"
                                        ]
                                    )
                                    KEmpty
                                )
                            , KEmpty
                            ]
                        )
                        (KVar "Rest")
                , kState = MapVar "M"
                }
        , sideCondition = B True
        }

rewriteRules :: [RewriteRule]
rewriteRules =
    [ assign
    , ifTrue
    , ifFalse
    , while
    ]
