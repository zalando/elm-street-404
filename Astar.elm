module Astar (astar) where


type alias Node =
  { tile: (Int, Int)
  , obstacle: Bool
  , parent: Maybe (Int, Int)
  , f: Float
  , g: Float
  , h: Float
  }


type alias NodeList = List Node
type alias Row = NodeList
type alias Grid = List Row


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


nodeEq : Node -> Node -> Bool
nodeEq a b = a.tile == b.tile


absDistance : Node -> Node -> Float
absDistance a b =
  ((toFloat (nodex a) - toFloat (nodex b)) ^ 2) +
  ((toFloat (nodey a) - toFloat (nodey b)) ^ 2)


heuristics : Node -> Node -> Float
heuristics node dest =
  (abs ((nodex node) - (nodex dest))) +
  (abs ((nodey node) - (nodey dest)))
  |> toFloat


contains : NodeList -> Node -> Bool
contains list node =
  case list of
    [] -> False
    first :: rest ->
      if nodeEq first node then
        True
      else
        contains rest node


setObstacle : NodeList -> Node -> Node
setObstacle obstacles node =
  { node | obstacle = contains obstacles node }


removeNode : NodeList -> Node -> NodeList
removeNode list node =
  case list of
    [] -> []
    first :: rest ->
      if nodeEq first node then
        rest
      else
        first :: removeNode rest node


createRow : Int -> Int -> NodeList -> Row
createRow rowIndex columns obstacles =
  case columns of
    0 -> []
    _ ->
      let
        node = setObstacle obstacles (toNode (rowIndex, columns - 1))
        rest = createRow rowIndex (columns - 1) obstacles
      in
        node :: rest


createGrid : (Int, Int) -> NodeList -> Grid
createGrid gridSize obstacles =
  case fst gridSize of
    0 -> []
    _ ->
      let
        row = createRow (fst gridSize - 1) (snd gridSize) obstacles
        rest = createGrid (fst gridSize - 1, snd gridSize) obstacles
      in
        row :: rest


getRowNode : Row -> (Int, Int) -> Maybe Node
getRowNode row tile =
  case row of
    [] -> Nothing
    n :: rest ->
      if (nodex n) == (fst tile) && (nodey n) == (snd tile) then
        Just n
      else
        getRowNode rest tile


getNode : Grid -> (Int, Int) -> Maybe Node
getNode grid tile =
  case grid of
    [] -> Nothing
    row :: rest ->
      let
        node = getRowNode row tile
      in
        case node of
          Nothing -> getNode rest tile
          Just n -> Just n


pathToTileList : Grid -> Maybe (Int, Int) -> List (Int, Int)
pathToTileList grid tile =
  case tile of
    Nothing -> []
    Just t ->
      let
        mn = getNode grid t
      in
        case mn of
          Nothing -> []
          Just n -> n.tile :: pathToTileList grid n.parent


smallestF : NodeList -> Maybe Node
smallestF nodes =
  List.foldl
    (\ el current ->
      case current of
        Nothing -> (Just el)
        Just c -> Just (if el.f < c.f then el else c))
    Nothing
    (nodes)


getNeighborsIter : Grid -> Node -> (Int, Int) -> NodeList
getNeighborsIter grid node tile =
  if (nodex node + 1) < (fst tile) then
    []
  else if (nodey node + 1) < (snd tile) then
    getNeighborsIter grid node (fst tile + 1, 0)
  else
    let
      current = getNode grid tile
    in
      case current of
        Nothing -> getNeighborsIter grid node (fst tile, snd tile + 1)
        Just c ->
          if nodeEq c node then
            getNeighborsIter grid node (fst tile, snd tile + 1)
          else
            c :: getNeighborsIter grid node (fst tile, snd tile + 1)


getNeighbors : Grid -> Node -> NodeList
getNeighbors grid node =
  (getNeighborsIter grid node (nodex node - 1, nodey node - 1))


filter : (Node -> Bool) -> NodeList -> NodeList
filter predicate list =
  case list of
    [] -> []
    first :: rest ->
      if predicate first then
        first :: filter predicate rest
      else
        filter predicate rest


updateNodeList : Row -> Node -> Row
updateNodeList row node =
  case row of
    [] -> []
    n :: rest ->
      if nodeEq n node then
        node :: rest
      else
        n :: updateNodeList rest node


updateGrid : Grid -> Node -> Grid
updateGrid grid node =
  case grid of
    [] -> []
    row :: rest -> updateNodeList row node :: updateGrid rest node


processNeighbors : Grid -> NodeList -> NodeList -> Node -> Node -> (Grid, NodeList)
processNeighbors grid open neighbors current dest =
  case neighbors of
    [] -> (grid, open)
    neighbor :: rest ->
      let
        g = current.g + absDistance current neighbor
        neighborOpen = contains open neighbor
        best = (not neighborOpen) || (g < neighbor.g)
        parent = if best then Just current.tile else neighbor.parent
        h = if (not neighborOpen) then heuristics neighbor dest else neighbor.h
        f = if best then g + h else neighbor.f
        nupdate = { neighbor
          | parent = parent
          , f = f
          , g = g
          , h = h
          }
        nextGrid = updateGrid grid nupdate
        nextOpen =
          if neighborOpen then
            updateNodeList open nupdate
          else
            nupdate :: open
      in
        processNeighbors nextGrid nextOpen rest current dest


estimateNext : Grid -> NodeList -> NodeList -> Node -> Node -> List (Int, Int)
estimateNext grid open closed dest current =
  let
    nextOpen = removeNode open current
    nextClosed = current :: closed
    neighbors = getNeighbors grid current
    freeNeighbors = filter (\ n -> not n.obstacle) neighbors
    openNeighbors = filter (\ n -> not (contains nextClosed n)) freeNeighbors
    gridAndOpen = processNeighbors grid nextOpen openNeighbors current dest
    continueGrid = fst gridAndOpen
    continueOpen = snd gridAndOpen
  in
    astarIter continueGrid continueOpen nextClosed dest


astarIter : Grid -> NodeList -> NodeList -> Node -> List (Int, Int)
astarIter grid open closed dest =
  case (smallestF open) of
    Nothing -> []
    Just c ->
      if nodeEq c dest then
        pathToTileList grid (Just c.tile)
      else
        estimateNext grid open closed dest c


clipFirst : List a -> List a
clipFirst list =
  case list of
    [] -> []
    _ :: rest -> rest


astar : (Int, Int) -> List (Int, Int) -> (Int, Int) -> (Int, Int) -> List (Int, Int)
astar gridSize obstacles start destination =
  let
    obstacleNodes = List.map toNode obstacles
    grid = createGrid gridSize obstacleNodes
    insideObstacle = contains obstacleNodes (toNode destination)
  in
    if insideObstacle then
      []
    else
      astarIter grid [toNode start] [] (toNode destination)
      |> List.reverse 
      |> clipFirst
