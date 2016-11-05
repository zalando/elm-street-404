module RequestView exposing (render)

import CategoryView
import Actions exposing (Action)
import Request exposing (Request)
import Box exposing (Box)
import Layers exposing (layers)
import MapObject exposing (MapObject)
import Textures


renderReturn : ( Float, Float ) -> Box
renderReturn position =
    Box.textured
        Textures.Categories
        position
        13
        ( layers.article
        , 1
        )


render : ( Float, Float ) -> MapObject -> Request -> List Box
render position house request =
    let
        renderClickable =
            Box.clickable ( 1, 1 ) ( 0, 0 ) position ( layers.clickAbove, 0 )
    in
        case request.category of
            Request.Return article ->
                renderClickable (Actions.ClickMapObject house (Just <| Actions.ClickArticle article))
                    :: if request.blinkHidden then
                        []
                       else
                        [ renderReturn position
                        , CategoryView.render position article.category
                        ]

            Request.Order category ->
                renderClickable (Actions.ClickMapObject house (Just <| Actions.ClickCategory category))
                    :: if request.blinkHidden then
                        []
                       else
                        [ CategoryView.render position category ]
