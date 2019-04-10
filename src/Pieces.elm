import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random exposing (generate, int, pair)
import Tuple exposing (first, second)

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

subscriptions : Piece -> Sub Msg
subscriptions model =
  Sub.none

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

-- clockwise | counter clockwise
type RotateDirection = CW | CCW

type Piece = Empty | Piece
    { direction : Direction
    , name : Name
    , vector : (Int, Int) }

type Msg = Spawn (Int, Int) | Move (Int, Int) | Rotate RotateDirection

init : () -> (Piece, Cmd Msg)
init _ =
    let 
        p = Empty
        msg = generate Spawn ( pair (int 0 6) (int 0 4) )
    in
        (p, msg)

-- UPDATE

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
getPosition p =
    case p of
        Piece { direction, name, vector } ->
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
        _ -> []

update : Msg -> Piece -> (Piece, Cmd Msg)
update msg p =
    case msg of
        Spawn (ptype, ppos) ->
            ( Piece
                { direction = getDirection ppos
                , name = getName ptype
                , vector = (0, 0) }
            , Cmd.none )
        _ -> (p, Cmd.none)

-- VIEW

view : Piece -> Html Msg
view p =
  div []
    [ div [] [ text ("kuku keke kaka") ] ]
