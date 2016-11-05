module View.EndGame exposing (render)

import Dict exposing (Dict)
import Box exposing (Box)
import Article exposing (Article)
import Customer exposing (Customer)
import View.Customer
import Textures
import Layers exposing (layers)
import Actions


renderCustomer : ( Float, Float ) -> List Article -> Int -> Customer -> List Box
renderCustomer ( x, y ) articles i customer =
    View.Customer.render articles ( x + toFloat i * 2, y + toFloat (i % 2) ) customer
        ++ [ Box.textured
                Textures.Spotlight
                ( x + toFloat i * 2 - 1, y + 2 + toFloat (i % 2) )
                0
                ( layers.shadow, 0 )
           ]


render : ( Int, Int ) -> Bool -> List Article -> Dict Int Customer -> List Box
render ( width, height ) isLost articles customersDict =
    let
        customers =
            if isLost then
                List.filter (\{ location } -> location == Nothing) (Dict.values customersDict)
            else
                List.filter .isDressed (Dict.values customersDict)

        frameOffset =
            if isLost then
                2
            else
                customers
                    |> List.head
                    |> Maybe.map .frame
                    |> Maybe.withDefault 0

        startX =
            (toFloat width - (toFloat (List.length customers * 2))) / 2

        startY =
            toFloat height / 4 * 3 - 2
    in
        [ Box.textured
            Textures.EndGame
            ( toFloat width / 2 - 5, toFloat height / 4 - 2 )
            frameOffset
            ( layers.clickToStart, 0 )
        , Box.clickable
            ( 10, 7 )
            ( 0, 0 )
            ( toFloat width / 2 - 5, toFloat height / 4 - 2 )
            ( layers.clickToStartAbove, 0 )
            Actions.BackToStart
        ]
            ++ (customers
                    |> List.indexedMap (renderCustomer ( startX, startY ) articles)
                    |> List.concat
               )
