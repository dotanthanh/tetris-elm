import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import List exposing (..)
import Html.Attributes exposing (..)

-- MAIN
 
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- Board

type alias Board = List (List Int)

init : () -> (Board, Cmd Msg)
init _ = (
  repeat 24 (repeat 10 0)
  , Cmd.none
  )

-- UPDATE

type Msg
  = Roll
  | NewFace Int


update : Msg -> Board -> (Board, Cmd Msg)
update msg board =
  case msg of
    _ -> (board, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Board -> Sub Msg
subscriptions board =
  Sub.none

-- VIEW

view : Board -> Html Msg
view board =
  div []
    [ div
        [ attribute "style" "background-color: black; width: 300px; height: 720px; margin: auto" ]
        (List.map (\grid -> div [attribute "style" "width: 30px; height: 30px;"] []) board) ]
