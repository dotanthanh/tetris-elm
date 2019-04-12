module Piece exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (generate, int, pair)
import Tuple exposing (first, second)
import String exposing (..)

-- STATIC INFo

type alias PieceStatus =
    { up : List (Int, Int)
    , down : List (Int, Int)
    , left : List (Int, Int)
    , right : List (Int, Int) }

iState : PieceStatus
iState =
    { up = [(-2, 3), (-2, 4), (-2, 5), (-2, 6)]
    , down = [(-2, 3), (-2, 4), (-2, 5), (-2, 6)]
    , left = [(-4, 4), (-3, 4), (-2, 4), (-1, 4)]
    , right = [(-4, 4), (-3, 4), (-2, 4), (-1, 4)] }

lState : PieceStatus
lState =
    { up = [(-3, 3), (-2, 3), (-1, 3), (-1, 4)]
    , down = [(-3, 3), (-3, 4), (-2, 4), (-1, 4)]
    , left = [(-1, 3), (-1, 4), (-1, 5), (-2, 5)]
    , right = [(-1, 3), (-2, 3), (-2, 4), (-2, 5)] }

jState : PieceStatus
jState =
    { up = [(-1, 4), (-1, 5), (-2, 5), (-3, 5)]
    , down = [(-1, 4), (-2, 4), (-3, 4), (-3, 5)]
    , left = [(-2, 3), (-2, 4), (-2, 5), (-1, 5)]
    , right = [(-2, 3), (-1, 3), (-1, 4), (-1, 5)] }

oState : PieceStatus
oState =
    { up = [(-2, 4), (-2, 5), (-1, 4), (-1, 5)]
    , down = [(-2, 4), (-2, 5), (-1, 4), (-1, 5)]
    , left = [(-2, 4), (-2, 5), (-1, 4), (-1, 5)]
    , right = [(-2, 4), (-2, 5), (-1, 4), (-1, 5)] }

sState : PieceStatus
sState =
    { up = [(-1, 3), (-1, 4), (-2, 4), (-2, 5)]
    , down = [(-1, 3), (-1, 4), (-2, 4), (-2, 5)]
    , left = [(-3, 4), (-2, 4), (-2, 5), (-1, 5)]
    , right = [(-3, 4), (-2, 4), (-2, 5), (-1, 5)] }

zState : PieceStatus
zState =
    { up = [(-2, 3), (-2, 4), (-1, 4), (-1, 5)]
    , down = [(-2, 3), (-2, 4), (-1, 4), (-1, 5)]
    , left = [(-1, 4), (-2, 4), (-2, 5), (-3, 5)]
    , right = [(-1, 4), (-2, 4), (-2, 5), (-3, 5)] }

tState : PieceStatus
tState =
    { up = [(-1, 3), (-1, 4), (-1, 5), (-2, 4)]
    , down = [(-2, 3), (-2, 4), (-2, 5), (-1, 4)]
    , left = [(-3, 4), (-2, 4), (-1, 4), (-2, 5)]
    , right = [(-2, 4), (-3, 5), (-2, 5), (-1, 5)] }

-- MODEL

type Name = I | O | T | S | Z | J | L

type Direction = Up | Down | Left | Right

type MoveOption = MoveLeft | MoveRight

-- clockwise | counter clockwise
type RotateDirection = CW | CCW

type alias Piece =
    { direction : Direction
    , name : Name
    , vector : (Int, Int) }

spawn : Int -> Int -> Piece
spawn a b = { direction = getDirection a, name = getName b, vector = (0, 0) }

-- HELPERS

getName : Int -> Name
getName n =
    case n of
        0 -> I
        1 -> L
        2 -> J
        3 -> Z
        4 -> S
        5 -> O
        _ -> T

getDirection : Int -> Direction
getDirection n =
    case n of
        0 -> Up
        1 -> Down
        2 -> Left
        _ -> Right

getPosition : Piece -> List (Int, Int)
getPosition { direction, name, vector } =
    let
        pieceState = case name of
            I -> iState
            L -> lState
            J -> jState
            Z -> zState
            S -> sState
            O -> oState
            _ -> tState
        piecePos = case direction of
            Right -> pieceState.right
            Down -> pieceState.down
            Left -> pieceState.left
            _ -> pieceState.up
    in
        List.map (\pos -> (first pos + first vector, second pos + second vector)) piecePos

isOut : Piece -> Bool
isOut p =
    let
        gridsOut = List.filter (\pos -> second pos < 0 || second pos > 9) (getPosition p)
    in
        List.length gridsOut > 0

-- UPDATE

rotate : RotateDirection -> Piece -> Piece
rotate d { direction, name, vector } =
    case d of
        CW -> case direction of
            Up -> { name = name, vector = vector, direction = Right }
            Right -> { name = name, vector = vector, direction = Down }
            Down -> { name = name, vector = vector, direction = Left }
            _ ->  { name = name, vector = vector, direction = Up }
        CCW -> case direction of
            Up -> { name = name, vector = vector, direction = Left }
            Left -> { name = name, vector = vector, direction = Down }
            Down -> { name = name, vector = vector, direction = Right }
            _ ->  { name = name, vector = vector, direction = Up }

move : MoveOption -> Piece -> Piece
move m ({ vector, name, direction } as p) =
    if isOut p then p
    else
        case m of
            MoveLeft -> { name = name, direction = direction, vector = (first vector, second vector - 1) }
            _ -> { name = name, direction = direction, vector = (first vector, second vector + 1) }

-- VIEW

size : Int
size = 30

getGridStyle : (Int, Int) -> String
getGridStyle (y, x) =
    let
        top = "top: " ++ fromInt (size * y) ++ "px;"
        left = "left: " ++ fromInt (size * x) ++ "px;"
    in
        "box-sizing: border-box; background-color: yellow; border: 1px solid black;position: absolute; width: " ++ fromInt size ++ "px;" ++ "height: " ++ fromInt size ++ "px;" ++ left ++ top

getPieceStyle : Piece -> List String
getPieceStyle p =
    List.map getGridStyle (getPosition p)

view : Piece -> Html msg
view p =
    div [attribute "style" "width: 100%; height: 100%; position: relative;"]
        (List.map (\style -> div [attribute "style" style] []) (getPieceStyle p))
