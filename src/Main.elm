module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Split



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
    = Value CellIndex
    | SubExpression CellIndex Operation Expression


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


fromCellIndex : CellIndex -> String
fromCellIndex ( row, col ) =
    col ++ String.fromInt row


isOperator : Char -> Bool
isOperator str =
    str == '+' || str == '-' || str == '/' || str == '*'


type alias ExpressionString =
    { firstOperand : String
    , firstOperator : String
    , restOfTheExpression : String
    }


initialExpressionString : ExpressionString
initialExpressionString =
    { firstOperand = ""
    , firstOperator = ""
    , restOfTheExpression = ""
    }


updateExpressionString : Char -> ExpressionString -> ExpressionString
updateExpressionString ch ({ firstOperand, firstOperator, restOfTheExpression } as exprStr) =
    let
        isOp =
            isOperator ch

        chSt =
            String.fromChar ch
    in
    case isOp of
        True ->
            case firstOperator of
                "" ->
                    { exprStr | firstOperator = chSt }

                _ ->
                    { exprStr | restOfTheExpression = restOfTheExpression ++ chSt }

        False ->
            case firstOperator of
                "" ->
                    { exprStr | firstOperand = firstOperand ++ chSt }

                _ ->
                    { exprStr | restOfTheExpression = restOfTheExpression ++ chSt }


splitStringIntoExpression : String -> ExpressionString
splitStringIntoExpression str =
    String.foldl
        updateExpressionString
        initialExpressionString
        str


toExpression : String -> Maybe Expression
toExpression exprString =
    let
        cellIndex =
            toCellIndex exprString
    in
    case cellIndex of
        Just c ->
            Just (Value c)

        Nothing ->
            let
                { firstOperand, firstOperator, restOfTheExpression } =
                    splitStringIntoExpression exprString

                operandIndex =
                    toCellIndex firstOperand

                operator =
                    toOperation firstOperator
            in
            Maybe.map3 (\c o cc -> SubExpression c o cc)
                operandIndex
                operator
                (toExpression restOfTheExpression)


toExpressionString : Expression -> String
toExpressionString expr =
    case expr of
        Value cellIndex ->
            fromCellIndex cellIndex

        SubExpression cellIndex operator restOfTheExpression ->
            fromCellIndex cellIndex ++ fromOperation operator ++ toExpressionString restOfTheExpression


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


getValueFromCellIndex : CellIndex -> Array { a | index : CellIndex, output : CellOutput } -> Maybe Float
getValueFromCellIndex cellIndex cells =
    cellIndex
        |> findCellFromCellIndex cells
        |> Maybe.map .output
        |> Maybe.andThen
            (\x ->
                case x of
                    PlainValue v ->
                        Just
                            v

                    ComputedValue v ->
                        Nothing
            )
        |> Maybe.andThen String.toFloat


evaluateExpression :
    Expression
    -> Array { a | index : CellIndex, output : CellOutput }
    -> Maybe Float
evaluateExpression expr cells =
    case expr of
        Value cellIndex ->
            getValueFromCellIndex
                cellIndex
                cells

        SubExpression cellIndex operator restOfTheExpr ->
            let
                valueOfFirstOperand =
                    getValueFromCellIndex cellIndex cells

                valueOfRestOfTheExpr =
                    evaluateExpression restOfTheExpr cells

                operands =
                    Maybe.map2 (\x y -> ( x, y )) valueOfFirstOperand valueOfRestOfTheExpr
            in
            Maybe.map
                (calculationOperation operator)
                operands


expressionOutput : Maybe Float -> String
expressionOutput vf =
    case vf of
        Just output ->
            String.fromFloat output

        Nothing ->
            "Error, couldn't evaluate expression"


expressionResult :
    Expression
    -> Array { a | index : CellIndex, output : CellOutput }
    -> String
expressionResult expr cells =
    evaluateExpression expr cells |> expressionOutput


resolveCellOutput : Array Cell -> Cell -> Cell
resolveCellOutput cells ({ value } as cell) =
    -- first character equals =, rest is expression string
    { cell
        | output =
            case toExpression (String.dropLeft 1 value) of
                Just v ->
                    ComputedValue v

                Nothing ->
                    PlainValue "Error, invalid expression"
    }


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


columnNames : List Char
columnNames =
    String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


noOfRows : Int
noOfRows =
    100


init : a -> ( Model, Cmd Msg )
init v =
    ( { cells =
            Array.fromList
                (List.range
                    1
                    noOfRows
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
                                columnNames
                        )
                )
      }
    , Cmd.none
    )


updateCellOutput : Array Cell -> Int -> Array Cell
updateCellOutput cells cellIndex =
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
                    if String.startsWith "=" cell.value then
                        resolveCellOutput cells cell

                    else
                        { cell | output = PlainValue cell.value }
            in
            Array.set cellIndex updatedCell cells


updateCellValue : Array { a | value : c } -> Int -> c -> Array { a | value : c }
updateCellValue cells cellIndex v =
    let
        cellToUpdate =
            Array.get cellIndex cells
    in
    case cellToUpdate of
        Nothing ->
            cells

        Just cell ->
            Array.set cellIndex { cell | value = v } cells


updateCellEditing : Array { a | editing : c } -> Int -> c -> Array { a | editing : c }
updateCellEditing cells cellIndex v =
    let
        cellToUpdate =
            Array.get cellIndex cells
    in
    case cellToUpdate of
        Nothing ->
            cells

        Just cell ->
            Array.set cellIndex { cell | editing = v } cells


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { cells } =
    case msg of
        CellValueChanged cellIndex value ->
            ( { cells = updateCellValue cells cellIndex value }, Cmd.none )

        CellEditable cellIndex ->
            ( { cells = updateCellEditing cells cellIndex True }, Cmd.none )

        CellUnEditable cellIndex ->
            let
                newCells =
                    updateCellOutput cells cellIndex
            in
            ( { cells = updateCellEditing newCells cellIndex False }, Cmd.none )


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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "spreadsheet"
        , model.cells
            |> spreadSheetView
            |> div [ style "overflow" "auto" ]
        ]


spreadSheetView : Array Cell -> List (Html Msg)
spreadSheetView cells =
    let
        rowsWithColumns =
            cells
                |> Array.toList
                |> List.Split.chunksOfLeft (List.length columnNames)
    in
    List.indexedMap
        (\rowIndex row -> rowView row rowIndex cells)
        rowsWithColumns


rowView : List Cell -> Int -> Array Cell -> Html Msg
rowView row rowIndex cells =
    div [ style "white-space" "nowrap" ]
        (List.indexedMap
            (\colIndex cell ->
                cellView cells cell (26 * rowIndex + colIndex)
            )
            row
        )


cellView : Array Cell -> Cell -> Int -> Html Msg
cellView cells cell index =
    input
        [ value <| showValue cell cells
        , onClick <| CellEditable index
        , onBlur <| CellUnEditable index
        , onInput <| CellValueChanged index
        ]
        []
