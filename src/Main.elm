module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



{-
   web spreadsheet program
   inspired by: https://aosabook.org/en/500L/web-spreadsheet.html
-}


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = CellValueChanged Int String
    | CellEditable Int
    | CellUnEditable Int


type Operation
    = Sum
    | Multiply
    | Subtract
    | Divide


type Expression
    = Expression CellIndex Operation CellIndex


toOperation : String -> Maybe Operation
toOperation rawString =
    case rawString of
        "+" ->
            Just Sum

        "-" ->
            Just Subtract

        "*" ->
            Just Multiply

        "/" ->
            Just Divide

        _ ->
            Nothing


fromOperation : Operation -> String
fromOperation op =
    case op of
        Sum ->
            "+"

        Subtract ->
            "-"

        Multiply ->
            "*"

        Divide ->
            "/"


toCellIndex : String -> Maybe CellIndex
toCellIndex rawString =
    let
        listOfChars =
            String.toList rawString

        ( column, row ) =
            List.partition Char.isAlpha listOfChars

        rowNo =
            String.fromList row

        columnNo =
            String.fromList column
    in
    case String.toInt rowNo of
        Just rowI ->
            Just ( rowI, columnNo )

        Nothing ->
            Nothing


isOperator : Char -> Bool
isOperator str =
    str == '+' || str == '-' || str == '/' || str == '*'



-- A11*B22


toExpression : String -> Maybe Expression
toExpression rawString =
    let
        exprString =
            String.dropLeft 1 rawString

        ( op, _ ) =
            exprString
                |> String.toList
                |> List.partition isOperator
                |> Tuple.mapFirst String.fromList

        ( c1, c2 ) =
            case String.split op exprString of
                [ y, z ] ->
                    ( Just y, Just z )

                _ ->
                    ( Nothing, Nothing )

        ops =
            toOperation op

        c11 =
            Maybe.andThen (\x -> toCellIndex x) c1

        c22 =
            Maybe.andThen (\x -> toCellIndex x) c2
    in
    Maybe.map3 (\x y z -> Expression x y z) c11 ops c22


toExpressionString : Expression -> String
toExpressionString (Expression ( r1, c1 ) op ( r2, c2 )) =
    "=" ++ c1 ++ String.fromInt r1 ++ fromOperation op ++ c2 ++ String.fromInt r2


findCellFromCellIndex :
    Array { a | index : CellIndex }
    -> CellIndex
    -> Maybe { a | index : CellIndex }
findCellFromCellIndex cells cellIndex =
    cells |> Array.filter (\x -> x.index == cellIndex) |> Array.get 0


calculationOperation : Operation -> ( Float, Float ) -> Float
calculationOperation operator ( op1, op2 ) =
    case operator of
        Sum ->
            op1 + op2

        Subtract ->
            op1 - op2

        Divide ->
            op1 / op2

        Multiply ->
            op1 * op2


evaluateExpression :
    Expression
    -> Array { a | index : CellIndex, output : CellOutput }
    -> Maybe Float
evaluateExpression (Expression operand1Index operator operand2Index) cells =
    let
        operand1 =
            operand1Index
                |> findCellFromCellIndex cells
                |> Maybe.map .output
                |> Maybe.andThen
                    (\x ->
                        case x of
                            PlainValue v ->
                                Just v

                            ComputedValue v ->
                                Nothing
                    )
                |> Maybe.andThen String.toFloat

        operand2 =
            operand2Index
                |> findCellFromCellIndex cells
                |> Maybe.map .output
                |> Maybe.andThen
                    (\x ->
                        case x of
                            PlainValue v ->
                                Just v

                            ComputedValue v ->
                                Nothing
                    )
                |> Maybe.andThen String.toFloat

        operands =
            Maybe.map2 (\x y -> ( x, y )) operand1 operand2
    in
    Maybe.map (calculationOperation operator) operands


expressionOutput : Maybe Float -> String
expressionOutput vf =
    case vf of
        Just output ->
            String.fromFloat output

        Nothing ->
            "Error"


expressionResult :
    Expression
    -> Array { a | index : CellIndex, output : CellOutput }
    -> String
expressionResult expr cells =
    evaluateExpression expr cells |> expressionOutput


resolveCellOutput : Array Cell -> Cell -> Cell
resolveCellOutput cells ({ value } as cell) =
    let
        outOrNothing =
            toExpression value

        o =
            case outOrNothing of
                Just v ->
                    ComputedValue v

                Nothing ->
                    PlainValue "Error"
    in
    { cell | output = o }


type alias CellIndex =
    ( Int, String )


type CellOutput
    = PlainValue String
    | ComputedValue Expression


type alias Cell =
    { index : CellIndex
    , value : String
    , output : CellOutput
    , editing : Bool
    }


type alias Model =
    { cells : Array Cell
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


characters : List Char
characters =
    String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


init : a -> ( Model, Cmd Msg )
init v =
    ( { cells =
            Array.fromList
                (List.range
                    1
                    10
                    |> List.concatMap
                        (\i ->
                            List.map
                                (\c ->
                                    { index = ( i, String.fromChar c )
                                    , value = ""
                                    , output = PlainValue ""
                                    , editing = False
                                    }
                                )
                                characters
                        )
                )
      }
    , Cmd.none
    )


updateCellValue cells cellIndex v =
    let
        cellToUpdate =
            Array.get cellIndex cells
    in
    case cellToUpdate of
        Nothing ->
            cells

        Just cell ->
            let
                updatedCell =
                    { cell | value = v }

                newCell =
                    if String.startsWith "=" v then
                        resolveCellOutput cells updatedCell

                    else
                        { updatedCell | output = PlainValue v }
            in
            Array.set cellIndex newCell cells


updateCellEditing cells cellIndex v =
    let
        cellToUpdate =
            Array.get cellIndex cells
    in
    case cellToUpdate of
        Nothing ->
            cells

        Just cell ->
            let
                newCell =
                    { cell | editing = v }
            in
            Array.set cellIndex newCell cells


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { cells } =
    case msg of
        CellValueChanged cellIndex value ->
            ( { cells = updateCellValue cells cellIndex value }, Cmd.none )

        CellEditable cellIndex ->
            ( { cells = updateCellEditing cells cellIndex True }, Cmd.none )

        CellUnEditable cellIndex ->
            ( { cells = updateCellEditing cells cellIndex False }, Cmd.none )


showValue : Cell -> Array Cell -> String
showValue cell cells =
    if cell.editing then
        cell.value

    else
        case cell.output of
            PlainValue v ->
                v

            ComputedValue expr ->
                expressionResult expr cells


view model =
    div []
        [ text "spreadsheet"
        , div [] <|
            Array.toList
                (Array.indexedMap
                    (\index cell ->
                        input
                            [ value <| showValue cell model.cells
                            , onClick <| CellEditable index
                            , onBlur <| CellUnEditable index
                            , onInput (\v -> CellValueChanged index v)
                            ]
                            []
                    )
                    model.cells
                )
        ]
