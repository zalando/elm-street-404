module View.Model exposing (render)

import Model exposing (Model, State(..))
import Article
import MapObject exposing (MapObject, MapObjectCategory(..))
import Textures
import Box exposing (Box)
import Customer exposing (Customer)
import Dict


-- Views:

import View.Tree
import View.Fountain
import View.House
import View.Customer
import View.EndGame
import View.Warehouse
import View.DeliveryPerson
import View.Inventory
import View.Score
import View.StartGame
import View.Digits


renderCustomer : Model -> Customer -> List Box
renderCustomer model customer =
    case customer.location of
        Just { position } ->
            View.Customer.render model.articles position customer

        Nothing ->
            []


renderMapObject : Model -> MapObject -> List Box
renderMapObject model mapObject =
    case mapObject.category of
        TreeCategory ->
            View.Tree.render mapObject

        FountainCategory fountain ->
            View.Fountain.render fountain mapObject

        HouseCategory _ ->
            View.House.render model.requests model.articles mapObject

        WarehouseCategory capacity ->
            View.Warehouse.render model.articles capacity mapObject


render : Model -> Model
render model =
    let
        ( texturedBoxes, clickableBoxes ) =
            Box.split (boxes model)
    in
        { model
            | texturedBoxes = List.sortBy .layer texturedBoxes
            , clickableBoxes = clickableBoxes
        }


boxes : Model -> List Box
boxes model =
    case model.state of
        Initialising ->
            []

        Suspended _ ->
            []

        Loading ->
            View.Digits.render
                ( toFloat (Tuple.first model.gridSize) / 2 + 1, toFloat (Tuple.second model.gridSize) / 2 )
                (Textures.loadedTextures model.textures)

        Stopped ->
            View.DeliveryPerson.render 0 model.deliveryPerson
                ++ View.StartGame.render model.gridSize

        Lost ->
            View.Score.render model.gridSize model.score model.maxLives (Model.countLives model)
                ++ View.EndGame.render model.gridSize True model.articles model.customers

        Won ->
            View.Score.render model.gridSize model.score model.maxLives (Model.countLives model)
                ++ View.EndGame.render model.gridSize False model.articles model.customers

        Playing ->
            View.Inventory.render model.gridSize model.articles
                ++ View.DeliveryPerson.render (List.length (List.filter Article.isPicked model.articles)) model.deliveryPerson
                ++ View.Score.render model.gridSize model.score model.maxLives (Model.countLives model)
                ++ List.concatMap (renderMapObject model) model.mapObjects
                ++ List.concatMap (renderCustomer model) (Dict.values model.customers)
