module DeliveryPersonView exposing (render)

import DeliveryPerson exposing (DeliveryPerson)
import Box exposing (Box)
import Textures
import Basics exposing (atan2)
import Layers exposing (layers)


calculateDirection : ( Float, Float ) -> Int
calculateDirection ( x, y ) =
    round (2 + atan2 y x * 4 / pi) % 8


direction : DeliveryPerson -> Int
direction { route, position } =
    case route of
        ( x, y ) :: rest ->
            calculateDirection
                ( toFloat x - fst position
                , toFloat y - snd position
                )

        [] ->
            0


boxesOffset : Int -> ( Float, Float )
boxesOffset direction =
    case direction of
        2 ->
            ( -0.5, -2 )

        3 ->
            ( -0.25, -2.25 )

        5 ->
            ( 0.25, -2.25 )

        6 ->
            ( 0.5, -2 )

        _ ->
            ( 0, -2 )


render : Int -> DeliveryPerson -> List Box
render numberOfBoxes deliveryPerson =
    let
        frame =
            direction deliveryPerson * 3 + deliveryPerson.frame

        boxes =
            [ Box.offsetTextured
                ( 0, -2 )
                Textures.DeliveryPersonBack
                deliveryPerson.position
                frame
                ( layers.obstacle, 0 )
            , Box.offsetTextured
                (boxesOffset (direction deliveryPerson))
                Textures.Boxes
                deliveryPerson.position
                ((4 - numberOfBoxes) * 6 + (direction deliveryPerson % 2) * 3 + deliveryPerson.frame)
                ( layers.obstacle, 2 )
            , Box.offsetTextured
                ( 0, -2 )
                Textures.DeliveryPersonFront
                deliveryPerson.position
                (if frame >= 9 && frame <= 17 then
                    frame - 6
                 else
                    0
                )
                ( layers.obstacle, 3 )
            ]
    in
        case deliveryPerson.location of
            DeliveryPerson.OnTheWayTo _ _ ->
                boxes

            _ ->
                [ Box.offsetTextured
                    ( 0, -2 )
                    Textures.DeliveryPersonBack
                    deliveryPerson.position
                    (24 + numberOfBoxes)
                    ( layers.obstacle, 0 )
                ]
