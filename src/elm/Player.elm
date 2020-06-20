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
    previousDir
        |> exit warrior map
        |> Maybe.orElse (proceed warrior map history previousDir)
        |> Maybe.map Warrior.Move
        |> Maybe.withDefault Warrior.Wait

exit : Warrior -> Map -> Direction -> Maybe Direction
exit warrior map previous =
    directions
        |> dropWhile ((/=) previous)
        |> find (List.any (Tile.isExit << Tuple.second) << look warrior map)

proceed : Warrior -> Map -> History -> Direction -> Maybe Direction
proceed warrior map history previous =
    let dirs = dropWhile ((/=) previous) directions
        shouldProceed tiles =
            canProceed tiles && (List.any (not << hasVisited warrior history << Tuple.first) (List.filter (not << Tile.isWall << Tuple.second) tiles))
    in
    dirs
        |> find (shouldProceed << look warrior map)
        |> Maybe.orElse (find (canProceed << look warrior map) dirs)

canProceed : List ( Coordinate, Tile ) -> Bool
canProceed xs = case xs of
    ( _, tile ) :: _ -> not (Tile.isWall tile)
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
