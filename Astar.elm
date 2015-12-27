module Astar (Grid, createGrid, findPath) where

import Dict exposing (Dict)


type alias Node =
  { tile: (Int, Int)
  , obstacle: Bool
  , parent: Maybe (Int, Int)
  , f: Float
  , g: Float
  , h: Float
  }


type alias NodeList = List Node
type alias Grid = Dict (Int, Int) Node


toNode : (Int, Int) -> Node
toNode t =
  { tile = t
  , obstacle = False
  , parent = Nothing
  , f = 0
  , g = 0
  , h = 0
  }


nodex : Node -> Int
nodex n = fst n.tile


nodey : Node -> Int
nodey n = snd n.tile


nodeq : Node -> Node -> Bool
nodeq a b = a.tile == b.tile


absDistance : Node -> Node -> Float
absDistance a b =
  ((toFloat (nodex a) - toFloat (nodex b)) ^ 2) +
  ((toFloat (nodey a) - toFloat (nodey b)) ^ 2)


heuristics : Node -> Node -> Float
heuristics node dest =
  (abs ((nodex node) - (nodex dest))) +
  (abs ((nodey node) - (nodey dest)))
  |> toFloat


contains : Node -> NodeList -> Bool
contains node list = List.any (nodeq node) list


removeNode : NodeList -> Node -> NodeList
removeNode list node = List.filter (\ n -> not (nodeq n node)) list


updateNodeList : NodeList -> Node -> NodeList
updateNodeList list node = List.map (\ n -> if nodeq n node then node else n) list


getAreaTiles : (Int, Int) -> (Int, Int) -> List (Int, Int)
getAreaTiles lower higher =
  if fst lower == fst higher || snd lower == snd higher then
    []
  else if fst lower + 1 == fst higher then
    lower :: getAreaTiles (fst lower, snd lower + 1) higher
  else
    (getAreaTiles lower (fst lower + 1, snd higher)) ++
    (getAreaTiles (fst lower + 1, snd lower) higher)


createGrid : List (Int, Int) -> (Int, Int) -> Grid
createGrid obstacles gridSize =
  getAreaTiles (0, 0) gridSize
  |> List.map toNode
  |> List.map (\ node -> { node | obstacle = List.member node.tile obstacles })
  |> List.foldl insertGridNode Dict.empty


insertGridNode : Node -> Grid -> Grid
insertGridNode node grid = Dict.insert node.tile node grid


updateGrid : Node -> Grid -> Grid
updateGrid node grid = Dict.update node.tile (\ n -> Just node) grid


getGridNode : Grid -> (Int, Int) -> Maybe Node
getGridNode grid tile = Dict.get tile grid


getNeighbors : Grid -> Node -> NodeList
getNeighbors grid node =
  getAreaTiles (nodex node - 1, nodey node - 1) (nodex node + 2, nodey node + 2)
  |> List.filter (\ tile -> not ((fst tile == nodex node) && (snd tile == nodey node)))
  |> List.filterMap (getGridNode grid)
  |> List.filter (\ node -> not node.obstacle)


smallestF : NodeList -> Maybe Node
smallestF nodes =
  List.foldl
    (\ el current ->
      case current of
        Nothing -> (Just el)
        Just c -> Just (if el.f < c.f then el else c))
    Nothing
    nodes


pathToTileList : Grid -> Maybe (Int, Int) -> List (Int, Int)
pathToTileList grid tile =
  case tile of
    Nothing -> []
    Just t ->
      let
        mn = getGridNode grid t
      in
        case mn of
          Nothing -> []
          Just n -> n.tile :: pathToTileList grid n.parent


processNeighbors : Grid -> NodeList -> NodeList -> Node -> Node -> (Grid, NodeList)
processNeighbors grid open neighbors current dest =
  case neighbors of
    [] -> (grid, open)
    neighbor :: rest ->
      let
        g = current.g + absDistance current neighbor
        neighborOpen = contains neighbor open
        best = (not neighborOpen) || (g < neighbor.g)
        parent = if best then Just current.tile else neighbor.parent
        h = if (not neighborOpen) then heuristics neighbor dest else neighbor.h
        f = if best then g + h else neighbor.f
        nupdate =
          { neighbor
          | parent = parent
          , f = f
          , g = g
          , h = h
          }
        nextGrid = updateGrid nupdate grid
        nextOpen =
          if neighborOpen then
            updateNodeList open nupdate
          else
            nupdate :: open
      in
        processNeighbors nextGrid nextOpen rest current dest


estimateNext : NodeList -> NodeList -> Node -> Node -> Grid -> List (Int, Int)
estimateNext open closed dest current grid =
  let
    nextOpen = removeNode open current
    nextClosed = current :: closed
    neighbors = getNeighbors grid current
    openNeighbors = List.filter (\ n -> not (contains n nextClosed)) neighbors
    gridAndOpen = processNeighbors grid nextOpen openNeighbors current dest
    continueGrid = fst gridAndOpen
    continueOpen = snd gridAndOpen
  in
    astar continueOpen nextClosed dest continueGrid


astar : NodeList -> NodeList -> Node -> Grid -> List (Int, Int)
astar open closed dest grid =
  case (smallestF open) of
    Nothing -> []
    Just c ->
      if nodeq c dest then
        pathToTileList grid (Just c.tile)
      else
        estimateNext open closed dest c grid


findPath : (Int, Int) -> (Int, Int) -> Grid -> List (Int, Int)
findPath start destination grid =
  let
    startNode = getGridNode grid start
    destNode = getGridNode grid destination
  in
    case (startNode, destNode) of
      (Nothing, _) -> []
      (_, Nothing) -> []
      (Just s, Just d) ->
        if d.obstacle then
          []
        else
          astar [s] [] d grid
          |> List.reverse
          |> List.drop 1
