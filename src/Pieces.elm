import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

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
init _ = Piece { direction = Up, name = I, vector = (0, 0)  }
    -- LOL wtf is this syntax
    -- state <- case name of
    --     I -> iState.up
    --     L -> lState.up
    --     J -> jState.up
    --     Z -> zState.up
    --     S -> sState.up
    --     O -> oState.up
    --     T -> tState.up

-- rotate : Piece -> RotateDirection -> Piece
-- rotate Piece {  } d = 


-- UPDATE

-- update : Msg -> Model -> Model
-- update msg model =
--   case msg of
--     Increment ->
--       model + 1

--     Decrement ->
--       model - 1


-- VIEW

-- view : Model -> Html Msg
-- view model =
--   div []
--     [ button [ onClick Decrement ] [ text "-" ]
--     , div [] [ text (String.fromInt model) ]
--     , button [ onClick Increment ] [ text "+" ]
--     ]