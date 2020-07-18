module Player exposing (takeTurn)

import Warrior exposing (Warrior, Action)
import Warrior.History exposing (History)
import Warrior.Map exposing (Map)

import AI exposing(nextAction)


takeTurn : Warrior -> Map -> History -> Action
takeTurn warrior map history =
    AI.nextAction warrior map history
