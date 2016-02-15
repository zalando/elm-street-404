module MapObjectCategory (MapObjectCategory, placeObjects, houses, warehouses, obstacles, deliveryPerson) where

import Random
import MapObject exposing (MapObject)
import House exposing (House)
import Warehouse exposing (Warehouse)
import Obstacle exposing (Obstacle)
import DeliveryPerson exposing (DeliveryPerson)


type MapObjectCategory
  = HouseCategory House
  | WarehouseCategory Warehouse
  | ObstacleCategory Obstacle
  | DeliveryPersonCategory DeliveryPerson


type alias Box = MapObject {}


placeObjects : Box -> DeliveryPerson -> List Warehouse -> List House -> List Obstacle -> Random.Generator (List MapObjectCategory)
placeObjects box deliveryPerson warehouses houses obstacles =
  let
    mapCategories =
      DeliveryPersonCategory deliveryPerson ::
      List.map WarehouseCategory warehouses ++
      List.map HouseCategory houses ++
      List.map ObstacleCategory obstacles
  in
    placeRandom mapCategories [box]


move : (Float, Float) -> MapObjectCategory -> MapObjectCategory
move position mapObject =
  case mapObject of
    HouseCategory house ->
      HouseCategory {house | position = position}
    WarehouseCategory warehouse ->
      WarehouseCategory {warehouse | position = position}
    ObstacleCategory obstacle ->
      ObstacleCategory {obstacle | position = position}
    DeliveryPersonCategory deliveryPerson ->
      DeliveryPersonCategory {deliveryPerson | position = position}


size : MapObjectCategory -> (Float, Float)
size mapObject =
  let
    (w, h) = size' mapObject
  in
    (w, h + 1)


size' : MapObjectCategory -> (Float, Float)
size' mapObject =
  case mapObject of
    HouseCategory house -> house.size
    WarehouseCategory warehouse -> warehouse.size
    ObstacleCategory obstacle -> obstacle.size
    DeliveryPersonCategory deliveryPerson -> deliveryPerson.size


placeRandom : List MapObjectCategory -> List Box -> Random.Generator (List MapObjectCategory)
placeRandom objects boxes =
  case objects of
    [] ->
      Random.map (always []) (Random.int 0 0)
    object :: restObjects ->
      case filterSize (size object) boxes of
        [] ->
          Random.map (always []) (Random.int 0 0)
        box :: restBoxes ->
          (fitRandom box (size object))
          `Random.andThen`
          (\position ->
            Random.map
              (\objects -> move position object :: objects)
              (placeRandom
                restObjects
                (sortBySize (splitBy {position = position, size = size object} box ++ restBoxes))
              )
          )


houses : List MapObjectCategory -> List House
houses categories =
  case categories of
    HouseCategory house :: rest -> house :: houses rest
    _ :: rest -> houses rest
    _ -> []


warehouses : List MapObjectCategory -> List Warehouse
warehouses categories =
  case categories of
    WarehouseCategory warehouse :: rest -> warehouse :: warehouses rest
    _ :: rest -> warehouses rest
    _ -> []


obstacles : List MapObjectCategory -> List Obstacle
obstacles categories =
  case categories of
    ObstacleCategory obstacle :: rest -> obstacle :: obstacles rest
    _ :: rest -> obstacles rest
    _ -> []


deliveryPerson : List MapObjectCategory -> Maybe DeliveryPerson
deliveryPerson categories =
  case categories of
    DeliveryPersonCategory deliveryPerson :: rest -> Just deliveryPerson
    _ :: rest -> deliveryPerson rest
    [] -> Nothing


splitBy : Box -> Box -> List Box
splitBy box1 box2 =
  let
    (x1, y1) = box1.position
    (w1, h1) = box1.size
    (x2, y2) = box2.position
    (w2, h2) = box2.size
  in
    List.filter
      (\{size} -> fst size > 0 && snd size > 0)
      [ {position = (x2, y2), size = (x1 - x2, w1 + y1 - y2)}
      , {position = (x1, y2), size = (x2 + w2 - x1, y1 - y2)}
      , {position = (x1 + w1, y1), size = (x2 + w2 - (x1 + w1), y2 + h2 - y1)}
      , {position = (x2, y1 + h1), size = (x1 + w1 - x2, y2 + h2 - (y1 + h1))}
      ]


fitRandom : Box -> (Float, Float) -> Random.Generator (Float, Float)
fitRandom {size, position} (w, h) =
  Random.map
    (\(x, y) -> (toFloat x, toFloat y))
    ( Random.pair
        (Random.int (floor (fst position)) (floor (fst position + fst size - w)))
        (Random.int (floor (snd position)) (floor (snd position + snd size - h)))
    )


filterSize : (Float, Float) -> List Box -> List Box
filterSize (w, h) =
  List.filter (\{size} -> fst size >= w && snd size >= h)


sortBySize : List Box -> List Box
sortBySize =
  List.sortBy (\{size} -> fst size * snd size) >> List.reverse
