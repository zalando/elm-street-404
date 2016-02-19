module Astar (findPath) where

import Dict exposing (Dict)


type alias Point = (Int, Int)


type alias NodeState =
  { node : Point
  , f : Float
  , g : Float
  , h : Float
  , parent : Maybe Point
  }


type alias State =
  { nodeStates : Dict Point NodeState
  , openNodes : List Point
  , closedNodes : List Point
  , gridSize : Point
  , dest : Point
  }


initialNodeState : Point -> NodeState
initialNodeState node =
  { node = node
  , f = 0
  , g = 0
  , h = 0
  , parent = Nothing
  }


initialState : Point -> List Point -> Point -> Point -> State
initialState gridSize obstacles start destination =
  { nodeStates = Dict.empty
  , openNodes = if List.all (onGrid gridSize) [start, destination] then [start] else []
  , closedNodes = obstacles
  , gridSize  = gridSize
  , dest = destination
  }


onGrid : Point -> Point -> Bool
onGrid (maxx, maxy) (x, y) = (x >= 0) && (y >= 0) && (x < maxx) && (y < maxy)


moveTile : Point -> Point -> Point
moveTile (tx, ty) (dx, dy) = (tx + dx, ty + dy)


absDistance : Point -> Point -> Float
absDistance (x1, y1) (x2, y2) =
  ((toFloat x1 - toFloat x2) ^ 2) +
  ((toFloat y1 - toFloat y2) ^ 2)


heuristics : Point -> Point -> Float
heuristics (x1, y1) (x2, y2) =
  (abs (x1 - x2)) + (abs (y1 - y2)) |> toFloat


nodeState : Point -> State -> Maybe NodeState
nodeState node {nodeStates} = Dict.get node nodeStates


checkInitNodeState : Point -> State -> State
checkInitNodeState node state =
  if onGrid state.gridSize node then
    case nodeState node state of
      Nothing ->
        { state
        | nodeStates = Dict.insert node (initialNodeState node) state.nodeStates
        }
      _ -> state
  else state


closeNode : Point -> State -> State
closeNode current state =
  { state
  | openNodes = List.filter ((/=) current) state.openNodes
  , closedNodes = current :: state.closedNodes
  }


getAreaTiles : Point -> Point -> List Point
getAreaTiles (x1, y1) (x2, y2) =
  if x1 == x2 || y1 == y2 then
    []
  else if x1 + 1 == x2 then
    (x1, y1) :: getAreaTiles (x1, y1 + 1) (x2, y2)
  else
    getAreaTiles (x1, y1) (x1 + 1, y2) ++
    getAreaTiles (x1 + 1, y1) (x2, y2)


freeUnclosedNeighbors : Point -> State -> List Point
freeUnclosedNeighbors node state =
  getAreaTiles (moveTile node (-1, -1)) (moveTile node (2, 2))
  |> List.filter ((/=) node)
  |> List.filter (onGrid state.gridSize)
  |> List.filter (\ n -> not (List.member n state.closedNodes))


processNeighbors : NodeState -> List Point -> State -> State
processNeighbors current neighbors state =
  case neighbors of
    [] -> state
    neighbor :: rest ->
      case nodeState neighbor state of
        Nothing -> processNeighbors current rest state
        Just nstate ->
          let
            g = current.g + absDistance current.node neighbor
            neighborOpen = List.member neighbor state.openNodes
            best = (not neighborOpen) || (g < nstate.g)
            parent = if best then Just current.node else nstate.parent
            h = if neighborOpen then nstate.h else heuristics neighbor state.dest
            f = if best then g + h else nstate.f
            nupdate =
              { nstate
              | parent = parent
              , f = f
              , g = g
              , h = h
              }
            nextState =
              { state
              | nodeStates = Dict.insert neighbor nupdate state.nodeStates
              , openNodes =
                  if neighborOpen then
                    state.openNodes
                  else
                    neighbor :: state.openNodes
              }
          in
            processNeighbors current rest nextState


processNodeNeighbors : NodeState -> State -> State
processNodeNeighbors nodeState state =
  let
    neighbors = freeUnclosedNeighbors nodeState.node state
    nextState = List.foldl checkInitNodeState state neighbors
  in
    processNeighbors nodeState neighbors nextState


astarIter : State -> State
astarIter state =
  let
    openNodeStates = List.filterMap ((flip nodeState) state) state.openNodes
  in
    case List.head (List.sortBy .f openNodeStates) of
      Nothing -> state
      Just nodeState ->
        if nodeState.node == state.dest then state
        else
          state
          |> closeNode nodeState.node
          |> processNodeNeighbors nodeState
          |> astarIter


unwindParents : Point -> State -> List Point
unwindParents node state =
  case nodeState node state `Maybe.andThen` .parent of
    Nothing -> [node]
    Just p -> node :: unwindParents p state


getPath : State -> List Point
getPath state =
  state
  |> unwindParents state.dest
  |> List.reverse
  |> List.drop 1


findPath : Point -> List Point -> Point -> Point -> List Point
findPath gridSize obstacles start destination =
  initialState gridSize obstacles start destination
  |> (flip (List.foldl checkInitNodeState)) [start, destination]
  |> astarIter
  |> getPath
