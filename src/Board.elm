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

type alias Board =
    { board: List (List Int)
    , piece: Maybe Piece }

init : () -> (Board, Cmd Msg)
init _ = (
    { board = repeat 24 (repeat 10 0), piece = Nothing }
    , Random.generate Spawn (Random.pair (Random.int 0 3) (Random.int 0 6)) 
    )

-- UPDATE

type Msg = Clear | Spawn (Int, Int) | Moving

update : Msg -> Board -> (Board, Cmd Msg)
update msg { board, piece } =
    case msg of
        Clear -> ({ board = board, piece = Nothing }, Random.generate Spawn (Random.pair (Random.int 0 3) (Random.int 0 6)))
        Spawn (a, b) -> ({ board = board, piece = spawn a b |> Just }, Cmd.none)
        _ -> case piece of
            Just p ->
                if shouldStop p then update Clear ({ board = board, piece = piece })
                else
                    ({ board = board, piece = fall p |> Just }, Cmd.none)
            _ -> ({ board = board, piece = piece }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Board -> Sub Msg
subscriptions board =
    Time.every 500 (\_ -> Moving)
-- VIEW

view : Board -> Html Msg
view { board, piece } =
    div [ attribute "style" "display: flex; border: 1px solid black; width: 300px; height: 600px; margin: auto;" ]
        (case piece of
            Just p -> [ div [] [ Piece.view p ] ]
            _ -> [])

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
