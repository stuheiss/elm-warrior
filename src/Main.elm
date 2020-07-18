module Main exposing (main)

import Player
import Warrior.Map.Progression as Progression
import Warrior.Maps as Maps
import Warrior.Program as Warrior


main : Program () Warrior.Model Warrior.Msg
main =
    Warrior.program
        { maps = List.drop 0 <| Maps.all
        , players = [ ( "Player", Player.takeTurn ) ]
        , msPerTurn = 500
        , progressionFunction = Progression.reachExitPoint
        }
