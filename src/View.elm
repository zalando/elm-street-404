module View (view) where

import Actions exposing (Action)
import Html exposing (div, br, Html, text, button)
import Html.Attributes exposing (style)
import Model exposing (Model)
import Sprite
import ObstacleView
import HouseView
import WarehouseView
import DeliveryPersonView
import PathView
import InventoryView
import Article
import ScoreView
import StartGameView
import DigitsView
import Layers exposing (layers)


(=>) : a -> b -> (a, b)
(=>) = (,)


boxes : Signal.Address Action -> Model -> List Sprite.Box
boxes address model =
  StartGameView.render address model.gridSize model.state ++
    if model.state == Model.Initialising then
      []
    else if model.state == Model.Loading then
      DigitsView.render
        (toFloat (fst model.gridSize) / 2 + 1, toFloat (snd model.gridSize) / 2)
        (round (100 * (1 - toFloat (List.length model.images) / toFloat (List.length Model.images))))
    else
      List.concat (
        StartGameView.render address model.gridSize model.state ::
        InventoryView.render address model.gridSize model.articles ::
        ScoreView.render model.gridSize model.score model.maxLives (Model.countLives model) ::
        DeliveryPersonView.render (List.length (List.filter Article.isPicked model.articles)) model.deliveryPerson ::
        List.map (HouseView.render address model.requests model.articles model.customers) model.houses ++
        List.map (WarehouseView.render address model.articles) model.warehouses ++
        List.map ObstacleView.render model.obstacles
      )


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
        , "z-index" => toString layers.grid
        ]
    ]
    []


view : Signal.Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "height" => (toString (snd model.gridSize * model.tileSize) ++ "px")
    , "margin" => "auto"
    , "position" => "relative"
    , "width" => (toString (fst model.gridSize * model.tileSize) ++ "px")
    , "background-image" => ("url(" ++ model.imagesUrl ++ "/bg-tile.jpg" ++ ")")
    , "background-size" => "560px 560px"
    ]
  ]
  ( PathView.render model.gridSize model.tileSize model.deliveryPerson.route ::
    List.map (Sprite.render model.imagesUrl model.tileSize) (Sprite.sort (boxes address model))
  )
