import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import List exposing (..)
import Html.Attributes exposing (..)
import Piece exposing (..)
import Board exposing (..)
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

type alias GameState =
    { board: Board
    , piece: Maybe Piece
    , lost: Bool }

type alias PieceSeed =
    { name: Int
    , direction: Int
    , color: Int }

init : () -> (GameState, Cmd Msg)
init _ = (
    { board = repeat 20 (repeat 10 Transparent), piece = Nothing, lost = False }
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
    | Lost
    | Move MoveOption
    | Rotate
    | Augment
    | Reduce
    | Moving
    | None

update : Msg -> GameState -> (GameState, Cmd Msg)
update msg ({ board, piece } as gs) =
    case piece of
        Just p -> case msg of
            Spawn { direction, name, color } ->
                ({ gs | piece = spawn direction name color |> Just }
                , Cmd.none )
            Lost -> ({ gs | lost = True }, Cmd.none)
            Move m ->
                let
                    movedPiece = move m p |> Just
                    updatedBoard =
                        if isOverlap { gs | piece = movedPiece } then gs
                        else { gs | piece = movedPiece }
                in
                    (updatedBoard, Cmd.none)
            Rotate ->
                let
                    pie = rotate p
                    updatedBoard =
                        if isOverlap { gs | piece = Just pie } == False then
                            { gs | piece = Just pie }
                        else if isOverlap { gs | piece = move MoveLeft pie |> Just } == False then
                            { gs | piece = move MoveLeft pie |> Just }
                        else if isOverlap { gs | piece = move MoveRight pie |> Just } == False then
                            { gs | piece = move MoveRight pie |> Just }
                        else gs
                in
                    (updatedBoard, Cmd.none)
            Augment -> case augment gs of
                Just g -> update Reduce g
                _ -> update Lost { gs | piece = Nothing }  
            _ ->
                if isOverlap { gs | piece = fall p |> Just } then update Augment gs
                else ({ gs | piece = fall p |> Just } , Cmd.none)

        _ -> case msg of
            Lost -> ({ gs | lost = True }, Cmd.none)
            Reduce ->
                ({ gs | board = reduce board, piece = Nothing }
                , Cmd.none )
            Spawn { direction, name, color } -> ({ gs | piece = spawn direction name color |> Just }, Cmd.none)
            _ ->
                ( gs
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

augment : GameState -> Maybe GameState
augment g =
    case g.piece of
        Just p ->
            let
                lost = List.any (\(y, _) -> y < 0) (getPosition p)
                f y row =
                    List.indexedMap (\x grid -> if List.any (\n -> n == (y, x)) (getPosition p) then p.color else grid ) row
            in
                if lost then Nothing
                else Just { g | board = List.indexedMap f g.board, piece = Nothing }
        _ -> Just g

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
subscriptions { lost } =
    if lost then Sub.none
    else
        batch
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
        (List.foldl (++) [] (List.indexedMap f b)) |> div [ attribute "style" "z-index: -100; position: absolute; height: 100%; width: 100%; display: grid; grid-template-columns: repeat(10, 1fr); grid; grid-template-rows: repeat(20, 1fr); grid-gap: 1px;" ]


view : GameState -> Html Msg
view ({ board, piece, lost } as g) =
    let
        pieceView = case piece of
                Just p -> div [ attribute "style" "position: absolute; height: 100%; width: 100%; display: grid; grid-template-columns: repeat(10, 1fr); grid; grid-template-rows: repeat(20, 1fr); grid-gap: 1px;" ] (Piece.view p)
                _ -> div [] []
        status = if lost then "you lose, sucker" else ""
    in
        div [] [
            h4 [ attribute "style" "text-align: center;" ] [ text status ],
            div [ attribute "style" "position: relative; display: flex; border: 1px solid black; width: 300px; height: 600px; margin: auto;" ]
                [ pieceView, getBoard g ]
        ]

