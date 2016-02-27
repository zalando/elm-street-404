module MapObject
  ( Box
  , MapObject
  , MapObjectCategory(..)
  , placeRandom
  , tree
  , fountain
  , house
  , warehouse
  , isHouse
  , animate
  , splitBy
  , houseSlots
  , warehouseSlots
  ) where

import Random
import Fountain exposing (Fountain)
import Time exposing (Time)


type alias Box =
  { position : (Float, Float)
  , size : (Float, Float)
  }


type MapObjectCategory
  = HouseCategory Int
  | WarehouseCategory Int
  | FountainCategory Fountain
  | TreeCategory


type alias MapObject' a b =
  { a | category : b }


type alias MapObject =
  MapObject' Box MapObjectCategory


animate : Time -> MapObject -> MapObject
animate elapsed mapObject =
  case mapObject.category of
    FountainCategory fountain ->
      {mapObject | category = FountainCategory (Fountain.animate elapsed fountain)}
    _ ->
      mapObject


mapObject : (Float, Float) -> MapObjectCategory -> MapObject
mapObject size category =
  { position = (0, 0)
  , size = size
  , category = category
  }


tree : MapObject
tree = mapObject (3, 2) TreeCategory


fountain : MapObject
fountain = mapObject (3, 2) (FountainCategory Fountain.fountain)


house : MapObject
house = mapObject (2, 2) (HouseCategory 3)


warehouse : MapObject
warehouse = mapObject (4, 3) (WarehouseCategory 6)


warehouseSlots : List MapObject -> List MapObject
warehouseSlots mapObjects =
  case mapObjects of
    obj :: rest ->
      case obj.category of
        WarehouseCategory capacity ->
          (List.repeat (capacity - 1) obj) ++ warehouseSlots rest
        _ ->
          warehouseSlots rest
    [] ->
      []


houseSlots : List MapObject -> List MapObject
houseSlots mapObjects =
  case mapObjects of
    obj :: rest ->
      case obj.category of
        HouseCategory capacity ->
          (List.repeat capacity obj) ++ houseSlots rest
        _ ->
          houseSlots rest
    [] ->
      []


isHouse : MapObject -> Bool
isHouse {category} =
  case category of
    HouseCategory _ -> True
    _ -> False


size : MapObject -> (Float, Float)
size {size} =
  let
    (w, h) = size
  in
    (w + 1, h + 1)


placeRandom : List MapObject -> List Box -> Random.Generator (List MapObject)
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
              (\objects -> {object | position = position} :: objects)
              (placeRandom
                restObjects
                (sortBySize (splitBy {position = position, size = size object} box ++ restBoxes))
              )
          )


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
