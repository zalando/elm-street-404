module RequestView (render) where

import CategoryView
import Actions exposing (Action)
import Html.Events exposing (onClick)
import Request exposing (Request)
import Sprite
import Category


render : Signal.Address Action -> (Float, Float) -> Request -> List Sprite.Box
render address position request =
  case request of
    Request.Return _ article _ ->
      CategoryView.render position [] article.category ++
      CategoryView.render
        position
        [onClick address (Actions.ClickArticle article)]
        Category.Return

    Request.Order _ category _ ->
      CategoryView.render
        position
        [onClick address (Actions.ClickCategory category)]
        category
