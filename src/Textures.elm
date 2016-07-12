module Textures exposing
  ( Textures
  , TextureId(..)
  , filename
  , textures
  , loadTextures
  , loadedTextures
  )

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
  | DeliveryPersonBack
  | DeliveryPersonFront
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
  | Boxes


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
    DeliveryPersonBack -> "delivery-person.png"
    DeliveryPersonFront -> "obstructing-delivery-person.png"
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
    Boxes -> "boxes.png"


textures : Textures
textures =
  AllDict.fromList
    filename
    [ (Categories, initData (1, 1) 14)
    , (ClickToStart, initData (10, 2) 1)
    , (Customers, initData (2, 3) 18)
    , (DeliveryPersonFront, initData (2, 4) 29)
    , (DeliveryPersonBack, initData (2, 4) 29)
    , (Boxes, initData (2, 4) 29)
    , (ElmStreet404, initData (13, 2) 1)
    , (Fountain, initData (3, 2) 1)
    , (FountainShadow, initData (4, 2) 1)
    , (FountainSpring, initData (1, 2) 4)
    , (House, initData (2, 3) 1)
    , (HouseBubble 1, initData (3, 3) 1)
    , (HouseBubble 2, initData (3, 4) 1)
    , (HouseBubble 3, initData (3, 5) 1)
    , (HouseShadow, initData (3, 2) 1)
    , (InventoryBubble, initData (7, 3) 1)
    , (Scarves, initData (2, 3) 3)
    , (Score, initData (1, 1) 13)
    , (Shirts, initData (2, 3) 12)
    , (Shoes, initData (2, 3) 4)
    , (Tree, initData (3, 5) 1)
    , (Trousers, initData (2, 3) 3)
    , (Warehouse, initData (4, 4) 1)
    , (WarehouseBubble, initData (4, 5) 1)
    , (WarehouseShadow, initData (5, 4) 1)
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
  , frames : Int
  , texture : Maybe WebGL.Texture
  }


initData : (Float, Float) -> Int -> TextureData
initData size frames =
  { size = size
  , frames = frames
  , texture = Nothing
  }
