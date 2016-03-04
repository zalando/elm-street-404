module ArticleView (render) where

import CategoryView
import Actions exposing (Action)
import Html.Events exposing (onClick)
import Article exposing (Article)
import Sprite


render : Signal.Address Action -> (Float, Float) -> Article -> List Sprite.Box
render address position article =
  CategoryView.render
    position
    (Just (onClick address (Actions.ClickArticle article)))
    article.category
