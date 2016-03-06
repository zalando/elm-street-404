module Textures
  ( Textures
  , TextureId(..)
  , filename
  , textures
  , loadTextures
  , loadedTextures
  ) where

import AllDict exposing (AllDict)
import WebGL


type alias Textures = AllDict TextureId TextureData String


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


textures : Textures
textures =
  AllDict.fromList
    filename
    [ (Categories, initData (1, 1) (0, 0) 14)
    , (ClickToStart, initData (10, 2) (0, 0) 1)
    , (Customers, initData (2, 3) (0, 0) 18)
    , (DeliveryPerson, initData (2, 4) (0, -2) 29)
    , (ElmStreet404, initData (13, 2) (0, 0) 1)
    , (Fountain, initData (3, 2) (0, 0) 1)
    , (FountainShadow, initData (4, 2) (0, 1) 1)
    , (FountainSpring, initData (1, 2) (1, -1) 4)
    , (House, initData (2, 3) (0, -1) 1)
    , (HouseBubble 1, initData (3, 3) (-2, -1) 1)
    , (HouseBubble 2, initData (3, 4) (-2, -2) 1)
    , (HouseBubble 3, initData (3, 5) (-2, -3) 1)
    , (HouseShadow, initData (3, 2) (0, 1) 1)
    , (InventoryBubble, initData (7, 3) (0, 0) 1)
    , (Scarves, initData (2, 3) (0, 0) 3)
    , (Score, initData (1, 1) (0, 0) 13)
    , (Shirts, initData (2, 3) (0, 0) 12)
    , (Shoes, initData (2, 3) (0, 0) 4)
    , (Tree, initData (3, 5) (0, -3) 1)
    , (Trousers, initData (2, 3) (0, 0) 3)
    , (Warehouse, initData (4, 4) (0, -1) 1)
    , (WarehouseBubble, initData (4, 5) (-2, -3) 1)
    , (WarehouseShadow, initData (5, 4) (0, 0) 1)
    ]


loadedTextures : Textures -> Int
loadedTextures textures =
  ( 1 -
    toFloat (List.length (loadTextures textures)) /
    toFloat (AllDict.size textures)
  ) * 100 |> round


loadTextures : Textures -> List TextureId
loadTextures textures =
  AllDict.toList textures
  |> List.filter (\(id, data) -> data.texture == Nothing)
  |> List.map fst


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
