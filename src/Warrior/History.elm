module Warrior.History exposing
    ( History
    , roundsPlayed, previousStates, previousActions
    )

{-| Every warrior's turn is recorded into History. You can use this module to answer questions about the past.

@docs History

@docs roundsPlayed, previousStates, previousActions

-}

import Warrior exposing (Warrior)
import Warrior.Internal.History as Internal exposing (History)
import Warrior.Map exposing (Map)


{-| The History record. It contains all past actions, and the previous states of warriors and the map
-}
type alias History =
    Internal.History


{-| How many complete rounds have been played? A round is over when every warrior has taken an action.
-}
roundsPlayed : History -> Int
roundsPlayed =
    Internal.roundsPlayed


{-| Returns a list of the state of a warrior and the map, going back to the first turn. The first element in the list will represent the state of the warrior and the map at the beginning of that warrior's last turn.
-}
previousStates : Warrior -> History -> List ( Warrior, Map )
previousStates =
    Internal.previousStates


{-| A list of every action a warrior has ever taken. The first element will be the previous action, the last element the first action.
-}
previousActions : Warrior -> History -> List Warrior.Action
previousActions =
    Internal.previousActions
