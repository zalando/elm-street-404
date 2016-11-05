module View.Inventory exposing (render)

import Article exposing (Article)
import Box exposing (Box)
import Textures
import View.Category
import View.Placeholder
import Layers exposing (layers)
import Actions


render : ( Int, Int ) -> List Article -> List Box
render ( width, height ) articles =
    let
        x =
            toFloat (width - 7) / 2

        y =
            toFloat height - 3

        articlesInDelivery =
            List.filter Article.isPicked articles

        articlesNumber =
            List.length articlesInDelivery

        placeholders =
            [0..3 - articlesNumber]

        renderArticle number article =
            let
                pos =
                    ( toFloat number + x + 2, y + 1 )
            in
                [ View.Category.render pos article.category
                , Box.clickable ( 1, 1 ) ( 0, 0 ) pos ( layers.clickAbove, 0 ) (Actions.ClickArticle article)
                ]

        renderPlaceholder number =
            View.Placeholder.render
                ( toFloat (number + articlesNumber) + x + 2, y + 1 )

        renderCategory number =
            View.Category.render ( toFloat (number + articlesNumber) + x + 2, y + 1 )
    in
        Box.textured Textures.InventoryBubble ( x, y ) 0 ( layers.bubble, 0 )
            :: List.concat (List.indexedMap renderArticle articlesInDelivery)
            ++ List.map renderPlaceholder placeholders
