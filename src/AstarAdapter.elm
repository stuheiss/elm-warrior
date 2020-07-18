module AstarAdapter exposing (findPath, Position, Path)

import Astar exposing (findPath, Position, Path)
import Set exposing (Set)
import Dict exposing (Dict)
import Array exposing (Array)
import Warrior.Coordinate exposing (Coordinate)


type alias Position =
    ( Int, Int )


type alias Path =
    Array Coordinate


coordinateToPosition : Coordinate -> Position
coordinateToPosition c = ( c.x, c.y )


positionToCoordinate : Position -> Coordinate
positionToCoordinate ( x, y ) = { x = x, y = y }



findPath : (Position -> Position -> Float) -> (Position -> Set Position) -> Coordinate -> Coordinate -> Maybe Path
findPath costFn moveFn start end =
    let
        path = Astar.findPath costFn moveFn (coordinateToPosition start) (coordinateToPosition end)
    in
    Maybe.map (\ary -> Array.map positionToCoordinate ary) path
