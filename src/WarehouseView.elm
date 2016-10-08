module WarehouseView exposing (render)

import MapObject exposing (MapObject)
import Box exposing (Box)
import Article exposing (Article)
import Category exposing (Category)
import CategoryView
import Actions exposing (Action)
import Layers exposing (layers)
import Textures


render : List Article -> Int -> MapObject -> List Box
render articles capacity ({position} as warehouse) =
  let
    (x, y) = position
    articlesInWarehouse = List.filter (Article.inWarehouse warehouse) articles
    numberOfArticles = List.length articlesInWarehouse
    placeholders = List.repeat (capacity - numberOfArticles) Category.Placeholder

    renderArticle number article =
      let
        pos = (toFloat (number % 2) + x - 1, toFloat (number // 2) + y - 2)
      in
        [ CategoryView.render pos article.category
        , Box.clickable (1, 1) (0, 0) pos (layers.clickAbove, 0) (Actions.ClickMapObject warehouse (Just <| Actions.ClickArticle article))
        ]

    renderCategory number =
      CategoryView.render
        ( toFloat ((numberOfArticles + number) % 2) + x - 1
        , toFloat ((numberOfArticles + number) // 2) + y - 2
        )
  in
    [ Box.offsetTextured (0, -1) Textures.Warehouse position 0 (layers.obstacle, 0)
    , Box.textured Textures.WarehouseShadow position 0 (layers.shadow, 0)
    , Box.clickable (4, 4) (0, -1) position (layers.click, 0) (Actions.ClickMapObject warehouse Nothing)
    , Box.offsetTextured (-2, -3) Textures.WarehouseBubble warehouse.position 0 (layers.bubble, 0)
    ]
    ++ List.concat (List.indexedMap renderArticle articlesInWarehouse)
    ++ List.indexedMap renderCategory placeholders
