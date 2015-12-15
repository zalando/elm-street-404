module View (view) where
import Actions exposing (Action)
import Html exposing (div, br, Html, text, button)
import Html.Attributes exposing (style)
import Model exposing (Model)
import Sprite
import Obstacle
import House
import Warehouse

(=>) : a -> b -> (a, b)
(=>) = (,)

boxes : Model -> List Sprite.Box
boxes model =
  List.concat (
    List.map House.render model.houses ++
    List.map Warehouse.render model.warehouses ++
    List.map Obstacle.render model.obstacles
  )

view : Signal.Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "height" => "560px"
    , "margin" => "auto"
    , "position" => "relative"
    , "width" => "960px"
    , "background-image" => "url(img/bg-grid.jpg)"
    , "background-size" => "960px 560px"
    ]
  ]
  [ div [] (List.map (Sprite.render model.tileSize) (Sprite.sort (boxes model)))
  ]
