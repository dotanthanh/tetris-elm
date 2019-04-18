import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import List exposing (..)
import Html.Attributes exposing (..)
import Piece exposing (..)
import Time exposing (..)
import Json.Decode as Decode
import Browser.Events exposing (..)
import Platform.Sub exposing (batch)
import Debug exposing (..)

-- MAIN
 
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- GameState

type alias Board = List (List Color)

type alias GameState =
    { board: Board
    , piece: Maybe Piece }

type alias PieceSeed =
    { name: Int
    , direction: Int
    , color: Int }

init : () -> (GameState, Cmd Msg)
init _ = (
    { board = repeat 20 (repeat 10 Transparent), piece = Nothing }
    , Random.generate Spawn (Random.map3 PieceSeed (Random.int 0 3) (Random.int 0 6) (Random.int 0 6)) 
    )

getGrid : (Int, Int) -> Board -> Color
getGrid (y, x) b =
    let
        f n = case head n of
            Just m -> m
            _ -> []
    in
        case b |> (head << drop x << f << drop y) of
            Just c -> c
            _ -> Transparent

-- UPDATE

type Control = ArrowUp | ArrowDown | ArrowLeft | ArrowRight | Other

type Msg =
    Spawn PieceSeed
    | Move MoveOption
    | Rotate
    | Augment
    | Reduce
    | CheckLose
    | Moving
    | None

update : Msg -> GameState -> (GameState, Cmd Msg)
update msg ({ board, piece } as gs) =
    case piece of
        Just p -> case msg of
            Spawn { direction, name, color } ->
                ( { board = board, piece = spawn direction name color |> Just }
                , Cmd.none )
            Move m -> ({ board = board, piece = move m p |> Just }, Cmd.none)
            Rotate -> ({ board = board, piece = rotate p |> Just }, Cmd.none)
            Augment -> update Reduce (augment gs)
            CheckLose -> (gs, Cmd.none)
            _ ->
                if isOverlap { gs | piece = fall p |> Just } then update Augment gs
                else ({ gs | piece = fall p |> Just } , Cmd.none)

        _ -> case msg of
            Reduce ->
                ( { board = reduce board, piece = Nothing }
                , Cmd.none )
            Spawn { direction, name, color } -> ({ board = board, piece = spawn direction name color |> Just }, Cmd.none)
            _ ->
                ( { board = board, piece = piece }
                , Random.generate Spawn (Random.map3 PieceSeed (Random.int 0 3) (Random.int 0 6) (Random.int 0 6)))

isOverlap : GameState -> Bool
isOverlap { board, piece } =
    case piece of
        Just p ->
            let
                piecePos = getPosition p
            in
                List.any (\(y, x) -> getGrid (y, x) board /= Transparent || y > 19 || x < 0 || x > 9) piecePos
        _ -> False

augment : GameState -> GameState
augment g =
    case g.piece of
        Just p ->
            let
                f y row =
                    List.indexedMap (\x grid -> if List.any (\n -> n == (y, x)) (getPosition p) then p.color else grid ) row
            in
                { board = List.indexedMap f g.board, piece = Nothing }
        _ -> g

reduce : Board -> Board
reduce b =
    case length b of
        0 -> b
        _ ->
            let
                reverseB = reverse b
                maybeR = head reverseB
                f row = all (\c -> c /= Transparent) row
            in
                case maybeR of
                    Just r ->
                        if f r then
                            reverseB |> (\n -> (repeat 10 Transparent) :: n) << reduce << reverse << drop 1
                        else
                            reverseB |> (\n -> n ++ [r]) << reduce << reverse << drop 1
                    _ -> b
                

-- SUBSCRIPTIONS

subscriptions : GameState -> Sub Msg
subscriptions board = batch
    [ Time.every 500 (\_ -> Moving)
    , onKeyPress keyDecoder ]

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map (controlToMsg << toControl) (Decode.field "key" Decode.string)

controlToMsg : Control -> Msg
controlToMsg c =
    case c of
        ArrowUp -> Rotate
        ArrowDown -> Moving
        ArrowLeft -> Move MoveLeft
        ArrowRight -> Move MoveRight
        _ -> None

toControl : String -> Control
toControl key =
    case key of
        "ArrowUp" -> ArrowUp
        "ArrowDown" -> ArrowDown
        "ArrowRight" -> ArrowRight
        "ArrowLeft" -> ArrowLeft
        "w" -> ArrowUp
        "s" -> ArrowDown
        "a" -> ArrowLeft
        "d" -> ArrowRight 
        _ -> Other

-- VIEW

getBoard : GameState -> Html Msg
getBoard g =
    let
        b = g.board
        f y row = List.indexedMap (\x grid -> div [ attribute "style" (getGridStyle grid (y, x))] []) row
    in
        (List.foldl (++) [] (List.indexedMap f b)) |> div [ attribute "style" "position: absolute; height: 100%; width: 100%; z-index: -100;" ]


view : GameState -> Html Msg
view ({ board, piece } as g) =
    let
        pieceView = case piece of
                Just p -> div [] [ div [] [ Piece.view p ] ]
                _ -> div [] []
    in
        div [ attribute "style" "display: flex; border: 1px solid black; width: 300px; height: 600px; margin: auto;" ]
            [ pieceView, getBoard g ]
