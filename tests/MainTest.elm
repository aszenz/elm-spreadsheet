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
        , describe "toExpression"
            [ test "works" <|
                \_ ->
                    "=A1*A2"
                        |> Main.toExpression
                        |> Expect.equal
                            (Just
                                (Main.Expression
                                    ( 1, "A" )
                                    Main.Multiply
                                    ( 2, "A" )
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
            [ let
                testCells =
                    Array.fromList
                        [ { index = ( 1, "A" )
                          , output = "123"
                          }
                        , { index = ( 2, "A" )
                          , output = "123"
                          }
                        ]

                expr =
                    Main.Expression ( 1, "A" ) Main.Sum ( 2, "A" )
              in
              test "works" <|
                \x ->
                    testCells
                        |> Main.evaluateExpression expr
                        |> Expect.equal (Just 246.0)
            ]
        ]
