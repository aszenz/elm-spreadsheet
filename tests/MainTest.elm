module MainTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


suite : Test
suite =
    describe "The main"
        [ describe "toCellIndex"
            [ test "works" <|
                \_ ->
                    "A1"
                        |> Main.toCellIndex
                        |> Expect.equal (Just ( 1, "A" ))

            -- -- fuzz runs the test 100 times with randomly-generated inputs!
            -- , fuzz string "restores the original string if you run it again" <|
            --     \randomlyGeneratedString ->
            --         randomlyGeneratedString
            --             |> String.reverse
            --             |> String.reverse
            --             |> Expect.equal randomlyGeneratedString
            ]
        , describe "splitStringIntoExpression"
            [ test "works" <|
                \_ ->
                    "A1*A2*A3"
                        |> Main.splitStringIntoExpression
                        |> Expect.equal
                            { firstOperator = "*"
                            , firstOperand = "A1"
                            , restOfTheExpression = "A2*A3"
                            }
            ]
        , describe "toExpression"
            [ test "works for simple unit" <|
                \_ ->
                    "A1"
                        |> Main.toExpression
                        |> Expect.equal
                            (Just
                                (Main.Value <|
                                    Main.CellIndex ( 1, "A" )
                                )
                            )
            , test "works for binary operations" <|
                \_ ->
                    "A1*A2"
                        |> Main.toExpression
                        |> Expect.equal
                            (Just
                                (Main.SubExpression
                                    (Main.CellIndex ( 1, "A" ))
                                    Main.Multiply
                                    (Main.Value <| Main.CellIndex ( 2, "A" ))
                                )
                            )
            , test "works for compound operations" <|
                \_ ->
                    "A1*A2-A3"
                        |> Main.toExpression
                        |> Expect.equal
                            (Just
                                (Main.SubExpression
                                    (Main.CellIndex ( 1, "A" ))
                                    Main.Multiply
                                    (Main.SubExpression
                                        (Main.CellIndex ( 2, "A" ))
                                        Main.Subtract
                                        (Main.Value (Main.CellIndex ( 3, "A" )))
                                    )
                                )
                            )
            , test "works for complex operations" <|
                \_ ->
                    "A1*A2-A3/A4*C3*N6"
                        |> Main.toExpression
                        |> Expect.equal
                            (Just
                                (Main.SubExpression
                                    (Main.CellIndex ( 1, "A" ))
                                    Main.Multiply
                                    (Main.SubExpression
                                        (Main.CellIndex ( 2, "A" ))
                                        Main.Subtract
                                        (Main.SubExpression
                                            (Main.CellIndex ( 3, "A" ))
                                            Main.Divide
                                            (Main.SubExpression
                                                (Main.CellIndex ( 4, "A" ))
                                                Main.Multiply
                                                (Main.SubExpression
                                                    (Main.CellIndex ( 3, "C" ))
                                                    Main.Multiply
                                                    (Main.Value (Main.CellIndex ( 6, "N" )))
                                                )
                                            )
                                        )
                                    )
                                )
                            )
            ]
        , describe "toOperation"
            [ test "works" <|
                \_ ->
                    "*"
                        |> Main.toOperation
                        |> Expect.equal (Just Main.Multiply)
            ]
        , describe "evaluateExpression"
            (let
                testCells =
                    Array.fromList
                        [ { index = ( 1, "A" )
                          , output = Main.PlainValue "123"
                          }
                        , { index = ( 2, "A" )
                          , output = Main.PlainValue "123"
                          }
                        , { index = ( 3, "A" )
                          , output = Main.PlainValue "2"
                          }
                        , { index = ( 5, "B" )
                          , output = Main.PlainValue "786"
                          }
                        , { index = ( 5, "C" )
                          , output = Main.PlainValue "-6"
                          }
                        ]
             in
             [ test "works for simple case" <|
                \_ ->
                    testCells
                        |> Main.evaluateExpression (Main.SubExpression (Main.CellIndex ( 1, "A" )) Main.Sum (Main.Value (Main.CellIndex ( 2, "A" ))))
                        |> Expect.equal (Just 246.0)
             , test "works for complex case" <|
                \_ ->
                    testCells
                        |> Main.evaluateExpression
                            (Main.SubExpression
                                (Main.CellIndex ( 1, "A" ))
                                Main.Sum
                                (Main.SubExpression
                                    (Main.CellIndex ( 2, "A" ))
                                    Main.Sum
                                    (Main.SubExpression
                                        (Main.CellIndex ( 5, "C" ))
                                        Main.Multiply
                                        (Main.Value (Main.CellIndex ( 3, "A" )))
                                    )
                                )
                            )
                        |> Expect.equal (Just 234.0)
             , test "works with constants" <|
                \_ ->
                    testCells
                        |> Main.evaluateExpression (Main.SubExpression (Main.CellIndex ( 1, "A" )) Main.Sum (Main.Value (Main.Constant 3)))
                        |> Expect.equal (Just 126)
             ]
            )
        , describe "resolveCellOutput and showValue"
            (let
                testCells =
                    Array.fromList
                        [ { index = ( 1, "A" )
                          , editing = False
                          , value = "123"
                          , output = Main.PlainValue "123"
                          }
                        , { index = ( 2, "A" )
                          , editing = False
                          , value = "123"
                          , output = Main.PlainValue "123"
                          }
                        , { index = ( 3, "A" )
                          , editing = False
                          , value = "2"
                          , output = Main.PlainValue "2"
                          }
                        , { index = ( 5, "B" )
                          , editing = False
                          , value = "786"
                          , output = Main.PlainValue "786"
                          }
                        , { index = ( 5, "C" )
                          , editing = False
                          , value = "-6"
                          , output = Main.PlainValue "-6"
                          }
                        ]

                cell =
                    { value = "=A1+A2+B5-B5-A3+2"
                    , output = Main.PlainValue ""
                    , index = ( 1, "C" )
                    , editing = False
                    }
             in
             [ test "works" <|
                \_ ->
                    let
                        resolvedCell =
                            Main.resolveCellOutput testCells cell

                        value =
                            Main.showValue resolvedCell testCells
                    in
                    Expect.equal value "250"
             ]
            )
        ]
