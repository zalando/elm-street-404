module Sprite
  ( TextureId(..)
  , TextureData
  , TexturedBoxData
  , ClickableBoxData
  , Box(..)
  , filename
  , split
  , box
  , clickable
  , renderClickable
  , loadedTextures
  , textures
  , loadTextures
  ) where

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


textures : AllDict TextureId TextureData String
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


loadedTextures : AllDict TextureId TextureData String -> Int
loadedTextures textures =
  ( 1 -
    toFloat (List.length (loadTextures textures)) /
    toFloat (AllDict.size textures)
  ) * 100 |> round


loadTextures : AllDict TextureId TextureData String -> List TextureId
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


type Box
  = Clickable ClickableBoxData
  | Textured TexturedBoxData


type alias ClickableBoxData =
  { position : (Float, Float)
  , size : (Float, Float)
  , offset : (Float, Float)
  , onClick : Html.Attribute
  , layer : (Int, Int)
  }


type alias TexturedBoxData =
  { position : (Float, Float)
  , textureId : TextureId
  , frame : Int
  , layer : (Int, Int)
  }


box : TextureId -> (Float, Float) -> Int -> (Int, Int) -> Box
box textureId position frame layer =
  Textured
    { position = position
    , textureId = textureId
    , frame = frame
    , layer = layer
    }


clickable : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Int, Int) -> Html.Attribute -> Box
clickable size offset position layer onClick =
  Clickable
    { position = position
    , size = size
    , offset = offset
    , layer = layer
    , onClick = onClick
    }


(=>) : a -> b -> (a, b)
(=>) = (,)


sortBoxData :
  List {a | layer : (Int, Int), position : (Float, Float)} ->
  List {a | layer : (Int, Int), position : (Float, Float)}
sortBoxData =
  List.sortBy (\{layer, position} -> (fst layer, snd position, snd layer))


split : List Box -> (List TexturedBoxData, List ClickableBoxData)
split boxes =
  let
    (textured, clickable) = split' boxes
  in
    (sortBoxData textured, sortBoxData clickable)


split' : List Box -> (List TexturedBoxData, List ClickableBoxData)
split' boxes =
  case boxes of
    [] -> ([], [])
    box :: rest ->
      let
        (restTextured, restClickable) = split rest
      in
        case box of
          Textured texturedBox ->
            (texturedBox :: restTextured, restClickable)
          Clickable clickableBox ->
            (restTextured, clickableBox :: restClickable)


backgroundOffset : Int -> Int -> (Float, Float) -> WebGL.Texture -> String
backgroundOffset tileSize frame (xSize, ySize) texture =
  let
    (tw, th) = WebGL.textureSize texture
    fw = (round xSize) * tileSize
    fh = (round ySize) * tileSize
    cols = tw // (fw * 2)
    x = -(frame % cols) * fw
    y = -(frame // cols) * fh
  in
    toString x ++ "px " ++ toString y ++ "px"


backgroundSize : WebGL.Texture -> String
backgroundSize texture =
  let
    (w, h) = WebGL.textureSize texture
    width = w // 2
    height = h // 2
  in
    toString width ++ "px " ++ toString height ++ "px"


renderClickable : Int -> ClickableBoxData -> Html.Html
renderClickable tileSize {position, size, offset, onClick} =
  div
  ( [ style
      [ "left" => "0"
      , "top" => "0"
      , "transform" => ("translate(" ++ (toString ((fst position + fst offset) * toFloat tileSize)) ++ "px," ++ (toString ((snd position + snd offset) * toFloat tileSize)) ++ "px)")
      , "position" => "absolute"
      , "width" => (toString (fst size * toFloat tileSize) ++ "px")
      , "height" => (toString (snd size * toFloat tileSize) ++ "px")
      ]
    , onClick
    ]
  )
  []
