import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import List exposing (..)
import Html.Attributes exposing (..)
import Piece exposing (..)
import Time exposing (..)

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

type Msg = Spawn | Moving

update : Msg -> Board -> (Board, Cmd Msg)
update msg board =
    case msg of
        Spawn -> (board, Cmd.none)
        _ -> (board, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Board -> Sub Msg
subscriptions board =
    Time.every Time.second update
-- VIEW

view : Board -> Html Msg
view board =
    div [ attribute "style" "display: flex; border: 1px solid black; width: 300px; height: 600px; margin: auto;" ]
        [ div [] [ Piece.view (Piece.spawn 1 4) ] ]

-- update : Msg -> Piece -> (Piece, Cmd Msg)
-- update msg p =
--     case msg of
--         Spawn (ptype, ppos) ->
--             ( Piece
--                 { direction = getDirection ppos
--                 , name = getName ptype
--                 , vector = (0, 0) }
--             , Cmd.none )
--         _ -> (p, Cmd.none)
