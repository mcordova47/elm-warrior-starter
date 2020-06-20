module Player exposing (takeTurn)

import List.Extra exposing (dropWhile, find)
import Maybe.Extra as Maybe
import Warrior exposing (Warrior)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.History as History exposing (History)
import Warrior.Map as Map exposing (Map)
import Warrior.Map.Tile as Tile exposing (Tile)
import Warrior.Coordinate exposing (Coordinate)


takeTurn : Warrior -> Map -> History -> Warrior.Action
takeTurn warrior map history =
    let previousDir = lastMove warrior history |> Maybe.withDefault Direction.Left
    in
    pickUpItem warrior map
        |> Maybe.orElse (retreat warrior map history)
        |> Maybe.orElse (heal warrior map)
        |> Maybe.orElse (attack warrior map previousDir)
        |> Maybe.orElse (exit warrior map previousDir)
        |> Maybe.orElse (proceed warrior map history previousDir)
        |> Maybe.withDefault Warrior.Wait

pickUpItem : Warrior -> Map -> Maybe Warrior.Action
pickUpItem warrior map =
    if Tile.isItem (Map.lookDown warrior map) then
        Just Warrior.Pickup
    else
        Nothing

retreat : Warrior -> Map -> History -> Maybe Warrior.Action
retreat warrior map history =
    Direction.all
        |> List.any (Maybe.withDefault False << Maybe.map (Tile.isWarrior << Tuple.second) << List.head << look warrior map)
        |> \nextToWarrior ->
                if nextToWarrior && Warrior.health warrior <= 3 then
                    Maybe.map (Warrior.Move << invert) (lastMove warrior history)
                else
                    Nothing

heal : Warrior -> Map -> Maybe Warrior.Action
heal warrior map =
    Direction.all
        |> List.any (Maybe.withDefault False << Maybe.map (Tile.isWarrior << Tuple.second) << List.head << look warrior map)
        |> \nextToWarrior ->
                if not nextToWarrior && Warrior.health warrior < Warrior.maxHealth warrior then
                    Just Warrior.Heal
                else
                    Nothing

exit : Warrior -> Map -> Direction -> Maybe Warrior.Action
exit warrior map previous =
    directions
        |> dropWhile ((/=) previous)
        |> find (List.any (Tile.isExit << Tuple.second) << look warrior map)
        |> Maybe.map Warrior.Move

attack : Warrior -> Map -> Direction -> Maybe Warrior.Action
attack warrior map previous =
    directions
        |> dropWhile ((/=) previous)
        |> find (Maybe.withDefault False << Maybe.map (Tile.isWarrior << Tuple.second) << List.head << look warrior map)
        |> Maybe.map Warrior.Attack

proceed : Warrior -> Map -> History -> Direction -> Maybe Warrior.Action
proceed warrior map history previous =
    let dirs = dropWhile ((/=) previous) directions
        shouldProceed tiles =
            canProceed tiles &&
            List.any (not << hasVisited warrior history << Tuple.first) (List.filter (not << Tile.isWall << Tuple.second) tiles)
    in
    dirs
        |> find (shouldProceed << look warrior map)
        |> Maybe.orElse (find (canProceed << look warrior map) dirs)
        |> Maybe.map Warrior.Move

canProceed : List ( Coordinate, Tile ) -> Bool
canProceed xs = case xs of
    ( _, tile ) :: _ -> Tile.canMoveOnto tile
    _ -> False

hasVisited : Warrior -> History -> Coordinate -> Bool
hasVisited warrior history coordinate =
    History.previousStates warrior history
        |> List.any ((==) coordinate << Warrior.position << Tuple.first)

look : Warrior -> Map -> Direction -> List ( Coordinate, Tile )
look warrior map dir =
    Map.look dir warrior map

lastMove : Warrior -> History -> Maybe Direction
lastMove warrior =
    List.head << List.filterMap moveDirection << History.previousActions warrior

moveDirection : Warrior.Action -> Maybe Direction
moveDirection action = case action of
    Warrior.Move dir -> Just dir
    _ -> Nothing

directions : List Direction
directions =
    [ Direction.Right
    , Direction.Down
    , Direction.Left
    , Direction.Up
    , Direction.Right
    , Direction.Down
    , Direction.Left
    ]

invert : Direction -> Direction
invert dir = case dir of
    Direction.Left -> Direction.Right
    Direction.Up -> Direction.Down
    Direction.Right -> Direction.Left
    Direction.Down -> Direction.Up
