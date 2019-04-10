import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias PieceStatus = {
    up : List (Int, Int),
    down : List (Int, Int),
    left : List (Int, Int),
    right : List (Int, Int)
}

IState : PieceStatus
IState = {
    up = [(-2, 3), (-2, 4), (-2, 5), (-2, 6)],
    down = [(-2, 3), (-2, 4), (-2, 5), (-2, 6)],
    left = [(-4, 4), (-3, 4), (-2, 4), (-1, 4)],
    right = [(-4, 4), (-3, 4), (-2, 4), (-1, 4)]
}

LState : PieceStatus
LState = {
    up = [(-3, 3), (-2, 3), (-1, 3), (-1, 4)],
    down = [(-3, 3), (-3, 4), (-2, 4), (-1, 4)],
    left = [(-1, 3), (-1, 4), (-1, 5), (-2, 5)],
    right = [(-1, 3), (-2, 3), (-2, 4), (-2, 5)]
}

JState : PieceStatus
JState = {
    up = [(-1, 4), (-1, 5), (-2, 5), (-3, 5)],
    down = [(-1, 4), (-2, 4), (-3, 4), (-3, 5)],
    left = [(-2, 3), (-2, 4), (-2, 5), (-1, 5)],
    right = [(-2, 3), (-1, 3), (-1, 4), (-1, 5)]
}

OState : PieceStatus
OState = {
    up = [(-2, 4), (-2, 5), (-1, 4), (-1, 5)],
    down = [(-2, 4), (-2, 5), (-1, 4), (-1, 5)],
    left = [(-2, 4), (-2, 5), (-1, 4), (-1, 5)],
    right = [(-2, 4), (-2, 5), (-1, 4), (-1, 5)]
}

SState : PieceStatus
SState = {
    up = [(-1, 3), (-1, 4), (-2, 4), (-2, 5)],
    down = [(-1, 3), (-1, 4), (-2, 4), (-2, 5)],
    left = [(-3, 4), (-2, 4), (-2, 5), (-1, 5)],
    right = [(-3, 4), (-2, 4), (-2, 5), (-1, 5)]
}

ZState : PieceStatus
ZState = {
    up = [(-2, 3), (-2, 4), (-1, 4), (-1, 5)],
    down = [(-2, 3), (-2, 4), (-1, 4), (-1, 5)],
    left = [(-1, 4), (-2, 4), (-2, 5), (-3, 5)],
    down = [(-1, 4), (-2, 4), (-2, 5), (-3, 5)]
}

TState : PieceStatus
TState = {
    up = [(-1, 3), (-1, 4), (-1, 5), (-2, 4)],
    down = [(-2, 3), (-2, 4), (-2, 5), (-1, 4)],
    left = [(-3, 4), (-2, 4), (-1, 4), (-2, 5)],
    right = [(-2, 4), (-3, 5), (-2, 5), (-1, 5)]
}

-- MODEL

type Name = I | O | T | S | Z | J | L

type Direction = Up | Down | Left | Right

-- clockwise | counter clockwise
type RotateDirection = CW | CCW

type alias Piece = { direction : Direction, name : Name, state: List (Int, Int) }

init : Piece
init name = Piece { direction = Up, name = name, state = state  }
    state = case name of
        I -> IState.up
        L -> LState.up
        J -> JState.up
        Z -> ZState.up
        S -> SState.up
        O -> OState.up
        T -> TState.up

rotate : Piece -> RotateDirection -> Piece
rotate Piece {  } d = 

init : Model
init =
  0


-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]