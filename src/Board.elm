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
    { board: List (List Bool)
    , piece: Maybe Piece }

type alias PieceSeed =
    { name: Int
    , direction: Int
    , color: Int }

init : () -> (Board, Cmd Msg)
init _ = (
    { board = repeat 24 (repeat 10 False), piece = Nothing }
    , Random.generate Spawn (Random.map3 PieceSeed (Random.int 0 3) (Random.int 0 6) (Random.int 0 6)) 
    )

-- UPDATE

type Msg = Clear | Spawn PieceSeed | Moving

update : Msg -> Board -> (Board, Cmd Msg)
update msg { board, piece } =
    case msg of
        Clear -> ({ board = board, piece = Nothing }, Random.generate Spawn (Random.map3 PieceSeed (Random.int 0 3) (Random.int 0 6) (Random.int 0 6)))
        Spawn { direction, name, color } -> ({ board = board, piece = spawn direction name color |> Just }, Cmd.none)
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
