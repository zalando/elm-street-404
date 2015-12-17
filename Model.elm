module Model (Model, initial, start, animate, navigateToWarehouse, navigateToHouse, State(..), timeoutRequests) where

import Random
import Time exposing (Time)
import AnimationState
import DeliveryPerson exposing (DeliveryPerson)
import Article exposing (Article)
import Request exposing (Request)
import Obstacle exposing (Obstacle)
import House exposing (House)
import Customer exposing (Customer)
import Warehouse exposing (Warehouse)
import Pathfinder exposing (obstacleTiles)
import IHopeItWorks

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
  , customers : List Customer
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
  , articles = []
  , requests = []
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
  , customers = []
  , warehouses =
    [ Warehouse.warehouse (19, 4)
    , Warehouse.warehouse (1, 10)
    ]
  }


start : Model -> Model
start model =
  let
    warehouseSlots = IHopeItWorks.exclude
      (List.concat (List.map (\w -> List.repeat w.capacity w) model.warehouses))
      (Article.warehouses model.articles)
    (articles, seed) = Article.dispatch 8 warehouseSlots model.seed
    categories = Article.availableCategories articles (Request.orderedCategories model.requests)
    houseSlots = IHopeItWorks.exclude
      (List.concat (List.map (\h -> List.repeat h.capacity h) model.houses))
      (List.map Request.house model.requests)
    (orders, seed') = Request.orders 4 houseSlots categories seed
  in
    { model
    | articles = articles
    , seed = seed'
    , requests = orders
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
navigateToWarehouse warehouse =
  navigateTo
    (DeliveryPerson.OnTheWayToWarehouse warehouse)
    (placeToLocation warehouse)


navigateToHouse : House -> Model -> Model
navigateToHouse house =
  navigateTo
    (DeliveryPerson.OnTheWayToHouse house)
    (placeToLocation house)


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


splitList : (a -> Bool) -> List a -> (List a, List a)
splitList predicate list =
  case list of
    [] -> ([], [])
    first :: rest ->
      let
        restSplit = splitList predicate rest
      in
        if predicate first then
          (first :: fst restSplit, snd restSplit)
        else
          (fst restSplit, first :: snd restSplit)


decHappinessIfHome : List House -> Customer -> Customer
decHappinessIfHome houses customer =
  case houses of
    [] -> customer
    house :: rest ->
      if Customer.livesHere house customer then
        Customer.decHappiness customer
      else
        decHappinessIfHome rest customer


decHappiness : List Request -> Customer -> Customer
decHappiness timeouted customer =
  decHappinessIfHome
    (List.map Request.house timeouted)
    customer


maxLives : Int
maxLives = 3


countLives : Model -> Int
countLives model =
  maxLives -
  (model.customers
    |> List.filter Customer.isLost
    |> List.length)


timeoutRequests : Model -> Model
timeoutRequests model =
  let
    livesBefore = countLives model
    (inTime, timeouted) = splitList Request.inTime model.requests
  in
    { model
    | requests = inTime
    , customers = List.map (decHappiness timeouted) model.customers
    }
