module View.House exposing (render)

import Actions exposing (Action)
import Layers exposing (layers)
import Box exposing (Box)
import MapObject exposing (MapObject)
import Request exposing (Request)
import Article exposing (Article)
import View.Request
import Textures


render : List Request -> List Article -> MapObject -> List Box
render requests articles ({ position } as house) =
    let
        requestsFromHouse =
            List.filter (\r -> r.house == house) requests

        renderRequest number =
            View.Request.render ( Tuple.first position - 1, Tuple.second position - toFloat number ) house

        renderBubble =
            case List.length requestsFromHouse of
                0 ->
                    []

                n ->
                    [ Box.offsetTextured ( -2, toFloat -n ) (Textures.HouseBubble n) house.position 0 ( layers.bubble, 0 ) ]
    in
        [ Box.offsetTextured ( 0, -1 ) Textures.House position 0 ( layers.obstacle, 0 )
        , Box.offsetTextured ( 0, 1 ) Textures.HouseShadow position 0 ( layers.shadow, 0 )
        , Box.clickable ( 2, 3 ) ( 0, -1 ) position ( layers.click, 0 ) (Actions.ClickMapObject house Nothing)
        , Box.clickable ( 2, 3 ) ( 0, 0 ) position ( layers.click, 0 ) (Actions.ClickMapObject house Nothing)
        ]
            ++ List.concat (List.indexedMap renderRequest requestsFromHouse)
            ++ renderBubble
