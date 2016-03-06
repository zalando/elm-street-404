module WarehouseView (render) where

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
      CategoryView.render
        ( toFloat (number % 2) + x - 1
        , toFloat (number // 2) + y - 2)
        (Just (Actions.ClickArticle article))
        article.category

    renderCategory number =
      CategoryView.render
        ( toFloat ((numberOfArticles + number) % 2) + x - 1
        , toFloat ((numberOfArticles + number) // 2) + y - 2
        )
        Nothing

    renderBubble =
      if List.length articlesInWarehouse == 0 then
        []
      else
        ( Box.textured
            Textures.WarehouseBubble
            warehouse.position
            0
            (layers.bubble, 0)
        ) ::
        List.concat (List.indexedMap renderArticle articlesInWarehouse) ++
        List.concat (List.indexedMap renderCategory placeholders)
  in
    [ Box.textured Textures.Warehouse position 0 (layers.obstacle, 0)
    , Box.textured Textures.WarehouseShadow position 0 (layers.shadow, 0)
    , Box.clickable (4, 4) (0, -1) position (layers.click, 0) (Actions.ClickMapObject warehouse)
    ]
    ++ renderBubble
