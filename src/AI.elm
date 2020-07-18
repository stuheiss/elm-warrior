module AI exposing (nextAction)

import Warrior.Direction as Direction exposing (Direction(..))
import Warrior.History as History exposing (History, roundsPlayed, previousStates, previousActions)
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Internal.Map as Map exposing (Map(..))
import Warrior exposing (Warrior, Action(..), position)
import Warrior.Map.Tile exposing (Tile(..))
import Warrior.Item exposing (Item)
import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Astar exposing (findPath, Position, Path)
import AstarAdapter exposing (findPath, Position, Path)


-- Astar uses Position
-- Warrior uses Coordinate
-- AstartAdapter transforms position/coordinate between Warrior and Astart


type alias Map =
    Map.Map


type alias Position =
    ( Int, Int )


type alias Coordinate =
    { x : Int, y : Int }


-- type alias Path =
--     Array Position


type alias World a =
    Dict Position a


type alias Cell = Tile


fst : ( a, b ) -> a
fst (a, b) = a


snd : ( a, b ) -> b
snd (a, b) = b


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b

canStandOn : Maybe Cell -> Bool
canStandOn cell =
    case cell of
        Nothing ->
            False

        Just Empty ->
            True

        Just SpawnPoint ->
            True

        Just Exit ->
            True

        Just (Item _) ->
            True

        Just (Warrior _) ->
            False

        Just Wall ->
            False


objectAt : World Cell -> Position -> Maybe Cell
objectAt =
    flip Dict.get


estimatedDistance : Position -> Position -> Float
estimatedDistance ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            toFloat <| abs (x1 - x2)

        dy =
            toFloat <| abs (y1 - y2)
    in
        abs <| (sqrt 2 * min dx dy) + abs (dy - dx)


movesFrom : Position -> Set Position
movesFrom ( x, y ) =
    Set.fromList
        [ ( x - 1, y )
        , ( x + 1, y )
        , ( x, y - 1 )
        , ( x, y + 1 )
        -- no diagonals allowed in this world
        -- , ( x - 1, y + 1 )
        -- , ( x - 1, y - 1 )
        -- , ( x + 1, y - 1 )
        -- , ( x + 1, y + 1 )
        ]


validMovesFrom : World Cell -> Position -> Set Position
validMovesFrom world position =
    Set.filter (canStandOn << objectAt world)
        (movesFrom position)


coordinateToPosition : Coordinate -> Position
coordinateToPosition c = ( c.x, c.y )


positionToCoordinate : Position -> Coordinate
positionToCoordinate ( x, y ) = { x = x, y = y }


backwards : Direction -> Direction
backwards dir =
    case dir of
        Left -> Right
        Right -> Left
        Up -> Down
        Down -> Up


nextAction : Warrior -> Map -> History -> Action
nextAction warrior (Map fields) history =
    let

        tiles : List Tile
        tiles = Array.toList fields.tiles

        tilesPerRow : Int
        tilesPerRow = fields.tilesPerRow

        indexes : List Int
        indexes = List.range 0 (List.length tiles - 1)

        worldList : List (Position, Tile)
        worldList = List.map2 (\index tile -> ((modBy tilesPerRow index, index // tilesPerRow), tile)) indexes tiles

        world : World Cell
        world = Dict.fromList worldList

        -- the start position for Astar
        startCoordinate : Coordinate
        startCoordinate = Warrior.position warrior


        -- the goal position for Astar
        goalCoordinate : Coordinate
        goalCoordinate = case List.filter (\(_, tile) -> tile == Exit) worldList of
           (pos, _)::_ -> positionToCoordinate pos
           _ -> startCoordinate


        -- shortest path from start to goal
        pathCoordinate : Maybe Path
        pathCoordinate =
            AstarAdapter.findPath
                estimatedDistance
                (validMovesFrom world)
                startCoordinate
                goalCoordinate

        pathPosition = Maybe.map (\ary -> Array.map coordinateToPosition ary) pathCoordinate

        -- next direction to move in the path
        nextDir : Coordinate -> Maybe Direction
        nextDir start = case pathPosition of
                Just ary -> case Array.get 0 ary of
                                Just (px, py) ->
                                    if px - start.x == 1 && py == start.y then Just Right
                                    else if px - start.x == -1 && py == start.y then Just Left
                                    else if py - start.y == 1 && px == start.x then Just Down
                                    else if py - start.y == -1 && px == start.x then Just Up
                                    else Nothing
                                _ -> Nothing
                _ -> Nothing


        prevMoves : List Action
        prevMoves = List.filter (\act -> case act of
                                                Move _ -> True
                                                _ -> False) <| History.previousActions warrior history


        -- return the last direction moved
        lastDir : Direction
        lastDir = case prevMoves of
                    Move d::_ -> d
                    _ -> Up


        -- maximum damage that can be inflicted assumes posession of Sword
        maxDamage = 3


        {-
        AI's advice on what to do next: pickup, move, fight, retreat, heal, or wait.
        1. if there is an item, pickup it up.
        2. else if there is an enemy 2 moves away and health is < maxHealth, heal.
        3. else if there is an enemy 1 move away and health is <= maxDamage, step back.
        4. else if there is an enemy 1 move away, attack.
        5. else if no next dir and health < maxHealth, heal.
        6. else if no next dir wait.
        -}
        action : Coordinate -> Action
        action start =
            case Map.lookDown warrior (Map fields) of
                Item _ -> Warrior.Pickup
                _ ->
                    case nextDir start of
                        Just dir -> case Map.look dir warrior (Map fields) of
                                    _::(_, Warrior _)::_ -> if Warrior.health warrior < Warrior.maxHealth warrior then Warrior.Heal else Warrior.Move dir
                                    (_, Warrior _)::_ -> if Warrior.health warrior <= maxDamage then Warrior.Move <| backwards lastDir
                                                        else Warrior.Attack dir
                                    _ -> if Warrior.health warrior < 10 then Warrior.Heal else Warrior.Move dir
                        _ -> if Warrior.health warrior < Warrior.maxHealth warrior then Warrior.Heal else Warrior.Wait
    in
    action startCoordinate
