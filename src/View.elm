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


(=>) : a -> b -> (a, b)
(=>) = (,)


boxes : Signal.Address Action -> Model -> List Sprite.Box
boxes address model =
  StartGameView.render address model.state ++
    if model.state == Model.Initialising then
      []
    else if model.state == Model.Loading then
      DigitsView.render (12, 6) (round (100 * (1 - toFloat (List.length model.images) / toFloat (List.length Model.images))))
    else
      List.concat (
        StartGameView.render address model.state ::
        InventoryView.render address model.articles ::
        ScoreView.render model.score model.maxLives (Model.countLives model) ::
        DeliveryPersonView.render (List.length (List.filter Article.isPicked model.articles)) model.deliveryPerson ::
        List.map (HouseView.render address model.requests model.articles model.customers) model.houses ++
        List.map (WarehouseView.render address model.articles) model.warehouses ++
        List.map ObstacleView.render model.obstacles
      )


view : Signal.Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "height" => (toString (snd model.gridSize * model.tileSize) ++ "px")
    , "margin" => "auto"
    , "position" => "relative"
    , "width" => (toString (fst model.gridSize * model.tileSize) ++ "px")
    , "background-image" => ("url(" ++ model.imagesUrl ++ "/bg-grid.jpg" ++ ")")
    , "background-size" => "960px 560px"
    ]
  ]
  ( PathView.render model.gridSize model.tileSize model.deliveryPerson.route ::
    List.map (Sprite.render model.imagesUrl model.tileSize) (Sprite.sort (boxes address model))
  )
