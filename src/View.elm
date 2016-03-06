module View (view) where

import Actions exposing (Action)
import Html exposing (div, br, Html, text, button)
import Html.Attributes exposing (style)
import Model exposing (Model)
import TreeView
import FountainView
import HouseView
import WarehouseView
import DeliveryPersonView
import PathView
import InventoryView
import Article
import ScoreView
import StartGameView
import DigitsView
import MapObject exposing (MapObject, MapObjectCategory(..))
import WebGLView
import ClickableView
import Textures
import Box exposing (Box)


(=>) : a -> b -> (a, b)
(=>) = (,)


renderMapObject : Model -> MapObject -> List Box
renderMapObject model mapObject =
  case mapObject.category of
    TreeCategory ->
      TreeView.render mapObject
    FountainCategory fountain ->
      FountainView.render fountain mapObject
    HouseCategory _ ->
      HouseView.render model.requests model.articles model.customers mapObject
    WarehouseCategory capacity ->
      WarehouseView.render model.articles capacity mapObject


boxes : Model -> List Box
boxes model =
  StartGameView.render model.gridSize model.state ++
  if model.state == Model.Initialising then
    []
  else if model.state == Model.Loading then
    DigitsView.render
      (toFloat (fst model.gridSize) / 2 + 1, toFloat (snd model.gridSize) / 2)
      (Textures.loadedTextures model.textures)
  else
    StartGameView.render model.gridSize model.state ++
    InventoryView.render model.gridSize model.articles ++
    ScoreView.render model.gridSize model.score model.maxLives (Model.countLives model) ++
    DeliveryPersonView.render (List.length (List.filter Article.isPicked model.articles)) model.deliveryPerson ++
    List.concat (List.map (renderMapObject model) model.mapObjects)


debug : Model -> Html
debug model =
  div
    [ style
        [ "background" => "linear-gradient(0deg, #000000 0, rgba(0,0,0,0) 2px, rgba(0,0,0,0) 100%), linear-gradient(90deg, #000000 0, rgba(0,0,0,0) 2px, rgba(0,0,0,0) 100%)"
        , "background-origin" => "padding-box"
        , "background-clip" => "border-box"
        , "background-size" => (toString model.tileSize ++ "px " ++ toString model.tileSize ++ "px")
        , "position" => "absolute"
        , "left" => "0"
        , "top" => "0"
        , "width" => "100%"
        , "height" => "100%"
        ]
    ]
    []


view : Signal.Address Action -> Model -> Html
view address model =
  let
    (texturedBoxes, clickableBoxes) = Box.split (boxes model)
    mapWidth = fst model.gridSize * model.tileSize
    mapHeight = snd model.gridSize * model.tileSize
    screenWidth = max (fst model.dimensions) mapWidth
    screenHeight = max (snd model.dimensions) mapHeight
  in
    div
      [ style
          [ "background-image" => ("url(" ++ model.imagesUrl ++ "/bg-tile.jpg" ++ ")")
          , "background-size" => "560px 560px"
          , "background-position" => "50% 50%"
          , "position" => "relative"
          , "width" => (toString screenWidth ++ "px")
          , "height" => (toString screenHeight ++ "px")
          ]
      ]
      [ div
          [ style
            [ "position" => "absolute"
            , "width" => (toString mapWidth ++ "px")
            , "height" => (toString mapHeight ++ "px")
            , "left" => (toString ((screenWidth - mapWidth) // 2) ++ "px")
            , "top" => (toString ((screenHeight - mapHeight) // 2) ++ "px")
            ]
          ]
          ( PathView.render model.gridSize model.tileSize model.deliveryPerson.route ::
            WebGLView.render model.gridSize model.tileSize model.textures texturedBoxes ::
            ClickableView.render address model.tileSize clickableBoxes
          )
      ]
