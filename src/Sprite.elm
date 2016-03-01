module Sprite (TextureId(..), TextureData, Box, box, empty, sort, render, textures) where

import Html.Attributes exposing (style, key)
import Html exposing (div)
import WebGL
import AllDict exposing (AllDict)


type TextureId
  = FountainSpring
  | HouseBubble Int
  | House
  | Customers
  | Tree
  | WarehouseShadow
  | Categories
  | DeliveryPerson
  | Fountain
  | InventoryBubble
  | Shirts
  | Trousers
  | Warehouse
  | ClickToStart
  | FountainShadow
  | HouseShadow
  | Scarves
  | Shoes
  | WarehouseBubble
  | ElmStreet404
  | Score
  | Transparent (Float, Float) (Float, Float)


filename : TextureId -> String
filename textureId =
  case textureId of
    FountainSpring -> "fountain-spring.png"
    HouseBubble n -> "house-bubble-" ++ toString n ++ ".png"
    House -> "house.png"
    Customers -> "customers.png"
    Tree -> "tree.png"
    WarehouseShadow -> "warehouse-shadow.png"
    Categories -> "categories.png"
    DeliveryPerson -> "delivery-person.png"
    Fountain -> "fountain.png"
    InventoryBubble -> "inventory-bubble.png"
    Shirts -> "shirts.png"
    Trousers -> "trousers.png"
    Warehouse -> "warehouse.png"
    ClickToStart -> "click-to-start.png"
    FountainShadow -> "fountain-shadow.png"
    HouseShadow -> "house-shadow.png"
    Scarves -> "scarves.png"
    Shoes -> "shoes.png"
    WarehouseBubble -> "warehouse-bubble.png"
    ElmStreet404 -> "404-elm-street.png"
    Score -> "score.png"
    Transparent _ _ -> ""


textures : AllDict TextureId TextureData String
textures =
  AllDict.fromList
    filename
    [ (FountainSpring, initData (1, 2) (1, -1) 4)
    , (HouseBubble 1, initData (3, 3) (-2, -1) 1)
    , (HouseBubble 2, initData (3, 4) (-2, -2) 1)
    , (HouseBubble 3, initData (3, 5) (-2, -3) 1)
    , (House, initData (2, 3) (0, -1) 1)
    , (Customers, initData (2, 3) (0, 0) 18)
    , (Tree, initData (3, 5) (0, -3) 1)
    , (WarehouseShadow, initData (5, 4) (0, 0) 1)
    , (Categories, initData (1, 1) (0, 0) 14)
    , (DeliveryPerson, initData (2, 3) (0, -1) 29)
    , (Fountain, initData (3, 2) (0, 0) 1)
    , (InventoryBubble, initData (7, 3) (0, 0) 1)
    , (Shirts, initData (2, 3) (0, 0) 12)
    , (Trousers, initData (2, 3) (0, 0) 3)
    , (Warehouse, initData (4, 4) (0, -1) 1)
    , (ClickToStart, initData (10, 2) (0, 0) 1)
    , (FountainShadow, initData (4, 2) (0, 1) 1)
    , (HouseShadow, initData (3, 2) (0, 1) 1)
    , (Scarves, initData (2, 3) (0, 0) 3)
    , (Shoes, initData (2, 3) (0, 0) 4)
    , (WarehouseBubble, initData (4, 5) (-2, -3) 1)
    , (ElmStreet404, initData (13, 2) (0, 0) 1)
    , (Score, initData (1, 1) (0, 0) 13)
    ]


type alias TextureData =
  { size : (Float, Float)
  , offset : (Float, Float)
  , frames : Int
  , texture : Maybe WebGL.Texture
  }


initData : (Float, Float) -> (Float, Float) -> Int -> TextureData
initData size offset frames =
  { size = size
  , offset = offset
  , frames = frames
  , texture = Nothing
  }


type alias Box =
  { position : (Float, Float)
  , textureId : TextureId
  , frame : Int
  , layer : (Int, Int)
  , attributes : List Html.Attribute
  }


box : TextureId -> (Float, Float) -> Int -> (Int, Int) -> Box
box textureId position frame layer =
  { position = position
  , textureId = textureId
  , frame = frame
  , layer = layer
  , attributes = []
  }


empty : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Int, Int) -> List Html.Attribute -> Box
empty size offset position layer attributes =
  { position = position
  , textureId = Transparent size offset
  , frame = 0
  , layer = layer
  , attributes = attributes
  }


(=>) : a -> b -> (a, b)
(=>) = (,)


sort : List Box -> List Box
sort =
  List.sortBy (\box -> (fst box.layer, snd box.position, snd box.layer))


render : String -> Int -> AllDict TextureId TextureData String -> Box -> Html.Html
render imagesUrl tileSize textures ({textureId, position, frame, layer, attributes}) =
  case textureId of
    Transparent size offset ->
      div
      ( [ style
          [ "left" => "0"
          , "top" => "0"
          , "transform" => ("translate(" ++ (toString ((fst position + fst offset) * toFloat tileSize)) ++ "px," ++ (toString ((snd position + snd offset) * toFloat tileSize)) ++ "px)")
          , "position" => "absolute"
          , "width" => (toString (fst size * toFloat tileSize) ++ "px")
          , "height" => (toString (snd size * toFloat tileSize) ++ "px")
          ]
        ] ++ attributes
      )
      []
    opaqueTextureId ->
      case AllDict.get opaqueTextureId textures of
        Nothing ->
          div [] []
        Just {size, offset, frames} ->
          div
          ( [ style
              [ "left" => "0"
              , "top" => "0"
              , "transform" => ("translate(" ++ (toString ((fst position + fst offset) * toFloat tileSize)) ++ "px," ++ (toString ((snd position + snd offset) * toFloat tileSize)) ++ "px)")
              , "position" => "absolute"
              , "overflow" => "hidden"
              , "background-image" => ("url(" ++ imagesUrl ++ "/" ++ (filename opaqueTextureId) ++ ")")
              , "background-position" => (toString -(frame * round (fst size) * tileSize) ++ "px 0")
              , "background-repeat" => "no-repeat"
              , "background-size" => (toString (round (fst size) * frames * tileSize) ++ "px " ++ (toString (round (snd size) * tileSize)) ++ "px")
              , "width" => (toString (round (fst size) * tileSize) ++ "px")
              , "height" => (toString (round (snd size) * tileSize) ++ "px")
              ]
            ] ++ attributes
          )
          []
