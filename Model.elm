module Model (Model, initial, animate, navigateToWarehouse, navigateToHouse, State(..)) where

import Random
import Time exposing (Time)
import AnimationState
import DeliveryPerson exposing (DeliveryPerson)
import Article exposing (Article)
import Request exposing (Request)
import Obstacle exposing (Obstacle)
import House exposing (House)
import Warehouse exposing (Warehouse)
import Pathfinder exposing (obstacleTiles)
import Category

type State = Paused | Playing | Stopped

type alias Model =
  { animationState : AnimationState.AnimationState
  , state : State
  , seed : Random.Seed
  , tileSize : Int
  , gridSize : (Int, Int)
  , deliveryPerson : DeliveryPerson
  , articles : List Article
  , requests : List Request
  , obstacles : List Obstacle
  , houses : List House
  , warehouses : List Warehouse
  }


initial : Model
initial =
  { animationState = Nothing
  , state = Stopped
  , seed = Random.initialSeed 0
  , tileSize = 40
  , gridSize = (24, 14)
  , deliveryPerson = DeliveryPerson.initial (10, 10)
  , articles =
    [ { category = fst (Category.random (Random.initialSeed 0))
      , state = Article.Picked
      , id = Random.initialSeed 0
      }
    , fst (Article.dispatch (Warehouse.warehouse (19, 4)) (Random.initialSeed 2))
    , fst (Article.dispatch (Warehouse.warehouse (19, 4)) (Random.initialSeed 3))
    , fst (Article.dispatch (Warehouse.warehouse (19, 4)) (Random.initialSeed 5))
    , fst (Article.dispatch (Warehouse.warehouse (1, 10)) (Random.initialSeed 1))
    ]
  , requests =
    [ Request.Order (House.house (8, 10)) (Category.Shirt 1) {elapsed = 0}
    , Request.Order (House.house (8, 10)) (Category.Shirt 2) {elapsed = 0}
    , Request.Order (House.house (8, 10)) (Category.Shirt 3) {elapsed = 0}
    ]
  , obstacles =
    [ Obstacle.fountain (10, 5)
    , Obstacle.fountain (20, 1)
    , Obstacle.tree (1, 5)
    , Obstacle.tree (15, 3)
    ]
  , houses =
    [ House.house (8, 10)
    , House.house (12, 7)
    , House.house (16, 10)
    , House.house (5, 5)
    ]
  , warehouses =
    [ Warehouse.warehouse (19, 4)
    , Warehouse.warehouse (1, 10)
    ]
  }


modelObstacles : Model -> List (Int, Int)
modelObstacles model =
  obstacleTiles model.obstacles ++
  obstacleTiles model.houses ++
  obstacleTiles model.warehouses


placeToLocation : {a | position : (Float, Float), size : (Float, Float )} -> (Int, Int)
placeToLocation {position, size} =
  ( round (fst position + snd size / 2 - 1)
  , round (snd position + snd size)
  )


navigateToWarehouse : Warehouse -> Model -> Model
navigateToWarehouse warehouse model =
  navigateTo
    (DeliveryPerson.OnTheWayToWarehouse warehouse)
    (placeToLocation warehouse)
    model


navigateToHouse : House -> Model -> Model
navigateToHouse house model =
  navigateTo
    (DeliveryPerson.OnTheWayToHouse house)
    (placeToLocation house)
    model


navigateTo : DeliveryPerson.Location -> (Int, Int) -> Model -> Model
navigateTo location destination model =
  { model
  | deliveryPerson = DeliveryPerson.navigateTo
      location
      model.gridSize
      (modelObstacles model)
      destination
      model.deliveryPerson
  }


animate : Time -> (Time -> Model -> Model) -> Model -> Model
animate time animationFunc model =
  let
    (elapsed, animationState) = AnimationState.animate time model.animationState
  in
    animationFunc elapsed {model | animationState = animationState}
