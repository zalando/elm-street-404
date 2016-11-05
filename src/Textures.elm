module Textures
    exposing
        ( Textures
        , TextureId(..)
        , filename
        , textures
        , loadTextures
        , loadedTextures
        )

import AllDict exposing (AllDict)
import WebGL


type alias Textures =
    AllDict TextureId TextureData String


type TextureId
    = FountainSpring
    | HouseBubble Int
    | House
    | Customers
    | Tree
    | WarehouseShadow
    | Categories
    | DeliveryPersonBack
    | DeliveryPersonFront
    | Fountain
    | InventoryBubble
    | Shirts
    | Trousers
    | Warehouse
    | ClickToStart
    | FountainShadow
    | HouseShadow
    | Scarves
    | Shoes
    | WarehouseBubble
    | ElmStreet404
    | Score
    | Boxes
    | EndGame
    | Heart
    | Spotlight


filename : TextureId -> String
filename textureId =
    case textureId of
        FountainSpring ->
            "fountain-spring.png"

        HouseBubble n ->
            "house-bubble-" ++ toString n ++ ".png"

        House ->
            "house.png"

        Customers ->
            "customers.png"

        Tree ->
            "tree.png"

        WarehouseShadow ->
            "warehouse-shadow.png"

        Categories ->
            "categories.png"

        DeliveryPersonBack ->
            "delivery-person.png"

        DeliveryPersonFront ->
            "obstructing-delivery-person.png"

        Fountain ->
            "fountain.png"

        InventoryBubble ->
            "inventory-bubble.png"

        Shirts ->
            "shirts.png"

        Trousers ->
            "trousers.png"

        Warehouse ->
            "warehouse.png"

        ClickToStart ->
            "click-to-start.png"

        FountainShadow ->
            "fountain-shadow.png"

        HouseShadow ->
            "house-shadow.png"

        Scarves ->
            "scarves.png"

        Shoes ->
            "shoes.png"

        WarehouseBubble ->
            "warehouse-bubble.png"

        ElmStreet404 ->
            "404-elm-street.png"

        Score ->
            "score.png"

        Boxes ->
            "boxes.png"

        EndGame ->
            "end-game.png"

        Heart ->
            "heart.png"

        Spotlight ->
            "spotlight.png"


textures : Textures
textures =
    AllDict.fromList
        filename
        [ ( Categories, TextureData ( 1, 1 ) 14 Nothing )
        , ( ClickToStart, TextureData ( 10, 2 ) 1 Nothing )
        , ( Customers, TextureData ( 2, 3 ) 18 Nothing )
        , ( DeliveryPersonFront, TextureData ( 2, 4 ) 29 Nothing )
        , ( DeliveryPersonBack, TextureData ( 2, 4 ) 29 Nothing )
        , ( Boxes, TextureData ( 2, 4 ) 29 Nothing )
        , ( ElmStreet404, TextureData ( 13, 2 ) 1 Nothing )
        , ( Fountain, TextureData ( 3, 2 ) 1 Nothing )
        , ( FountainShadow, TextureData ( 4, 2 ) 1 Nothing )
        , ( FountainSpring, TextureData ( 1, 2 ) 4 Nothing )
        , ( House, TextureData ( 2, 3 ) 1 Nothing )
        , ( HouseBubble 1, TextureData ( 3, 3 ) 1 Nothing )
        , ( HouseBubble 2, TextureData ( 3, 4 ) 1 Nothing )
        , ( HouseBubble 3, TextureData ( 3, 5 ) 1 Nothing )
        , ( HouseShadow, TextureData ( 3, 2 ) 1 Nothing )
        , ( InventoryBubble, TextureData ( 7, 3 ) 1 Nothing )
        , ( Scarves, TextureData ( 2, 3 ) 3 Nothing )
        , ( Score, TextureData ( 1, 1 ) 13 Nothing )
        , ( Shirts, TextureData ( 2, 3 ) 12 Nothing )
        , ( Shoes, TextureData ( 2, 3 ) 4 Nothing )
        , ( Tree, TextureData ( 3, 5 ) 1 Nothing )
        , ( Trousers, TextureData ( 2, 3 ) 3 Nothing )
        , ( Warehouse, TextureData ( 4, 4 ) 1 Nothing )
        , ( WarehouseBubble, TextureData ( 4, 5 ) 1 Nothing )
        , ( WarehouseShadow, TextureData ( 5, 4 ) 1 Nothing )
        , ( EndGame, TextureData ( 10, 7 ) 3 Nothing )
        , ( Heart, TextureData ( 2, 1 ) 2 Nothing )
        , ( Spotlight, TextureData ( 4, 2 ) 1 Nothing )
        ]


loadedTextures : Textures -> Int
loadedTextures textures =
    (1
        - toFloat (List.length (loadTextures textures))
        / toFloat (AllDict.size textures)
    )
        * 100
        |> round


loadTextures : Textures -> List TextureId
loadTextures textures =
    AllDict.toList textures
        |> List.filter (\( id, data ) -> data.texture == Nothing)
        |> List.map fst


type alias TextureWithSize =
    { size : ( Float, Float )
    , texture : WebGL.Texture
    }


type alias TextureData =
    { size : ( Float, Float )
    , frames : Int
    , texture : Maybe TextureWithSize
    }
