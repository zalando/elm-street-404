module View (view) where

import Actions exposing (Action)
import Html exposing (div, br, Html, text, button)
import Html.Attributes exposing (style)
import Model exposing (Model)
import Sprite
import Obstacle
import HouseView
import WarehouseView
import DeliveryPersonView
import PathView
import InventoryView
import Article
import ScoreView

(=>) : a -> b -> (a, b)
(=>) = (,)


boxes : Signal.Address Action -> Model -> List Sprite.Box
boxes address model =
  List.concat (
    DeliveryPersonView.render (List.length (List.filter Article.isPicked model.articles)) model.deliveryPerson ::
    List.map (HouseView.render address model.requests model.articles model.customers) model.houses ++
    List.map (WarehouseView.render address model.articles) model.warehouses ++
    List.map Obstacle.render model.obstacles ++
    [InventoryView.render address model.articles] ++
    [ScoreView.render model.score model.maxLives (Model.countLives model)]
  )


view : Signal.Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "height" => (toString (snd model.gridSize * model.tileSize) ++ "px")
    , "margin" => "auto"
    , "position" => "relative"
    , "width" => (toString (fst model.gridSize * model.tileSize) ++ "px")
    , "background-image" => "url(img/bg-grid.jpg)"
    , "background-size" => "960px 560px"
    ]
  ]
  ( PathView.render model.gridSize model.tileSize model.deliveryPerson.route ::
    List.map (Sprite.render model.tileSize) (Sprite.sort (boxes address model))
  )
