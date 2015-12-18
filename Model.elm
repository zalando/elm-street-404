module Model
  ( Model
  , initial
  , start
  , animate
  , navigateToWarehouse
  , navigateToHouse
  , State(..)
  , timeoutRequests
  , updateCustomers
  , updateGameState
  , deliverArticle
  , returnArticle
  , pickupReturn
  , pickupArticle
  , dispatchArticles
  , dispatchOrders
  , dispatchReturns
  , cleanupLostArticles
  , cleanupLostRequests
  ) where

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
import Article exposing (State(..), Article)
import Generator exposing (Generator)


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
  , orderGenerator : Generator
  , articleGenerator : Generator
  , returnGenerator : Generator
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
  , orderGenerator = Generator.initial 11000
  , articleGenerator = Generator.initial 13000
  , returnGenerator = Generator.initial 20000
  }


start : Model -> Model
start model =
  model |> dispatchCustomers
        |> dispatchArticles 6
        |> dispatchOrders 3


dispatchCustomers : Model -> Model
dispatchCustomers model =
  let
    (customers, seed) = Customer.rodnams model.houses model.seed
  in
    { model | customers = customers, seed = seed }


dispatchArticles : Int -> Model -> Model
dispatchArticles number model =
  let
    warehouseSlots = IHopeItWorks.exclude
      (List.concat (List.map (\w -> List.repeat w.capacity w) model.warehouses))
      (Article.warehouses model.articles)
    (articles, seed) = Article.dispatch number warehouseSlots model.seed
  in
    { model | articles = model.articles ++ articles, seed = seed }


dispatchOrders : Int -> Model -> Model
dispatchOrders number model =
  let
    categories = Article.availableCategories model.articles (Request.orderedCategories model.requests)
    houseSlots = IHopeItWorks.exclude
      (List.concat (List.map (\h -> List.repeat h.capacity h) model.houses))
      (List.map Request.house model.requests)
    (orders, seed) = Request.orders number houseSlots categories model.seed
  in
    { model | requests = model.requests ++ orders, seed = seed }


dispatchReturns : Int -> Model -> Model
dispatchReturns number model =
  model


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
    (inTime, timeouted) = splitList Request.inTime model.requests
  in
    { model
    | requests = inTime
    , customers = List.map (decHappiness timeouted) model.customers
    }


houseEmpty : List Customer -> House -> Bool
houseEmpty customers house =
  case customers of
    [] -> True
    customer :: otherCustomers ->
      if Customer.livesHere house customer then
        False
      else
        houseEmpty otherCustomers house


updateCustomers : Model -> Model
updateCustomers model =
  let
    emptyHouses = List.filter (houseEmpty model.customers) model.houses
    (newCustomers, seed) = Customer.rodnams emptyHouses model.seed
  in
    { model
    | customers = model.customers ++ newCustomers
    , seed = seed
    }


articleInEmptyHouse : List Customer -> Article -> Bool
articleInEmptyHouse customers article =
  case article.state of
    Article.Delivered house ->
      houseEmpty customers house
    _ -> False


requestInEmptyHouse : List Customer -> Request -> Bool
requestInEmptyHouse customers request =
  case request of
    Request.Order house _ _ -> houseEmpty customers house
    Request.Return house _ _ -> houseEmpty customers house


cleanupLostArticles : Model -> Model
cleanupLostArticles model =
  { model
  | articles =
      List.filter
        (\ article -> not (articleInEmptyHouse model.customers article))
        model.articles
  }


cleanupLostRequests : Model -> Model
cleanupLostRequests model =
  { model
  | requests =
      List.filter
        (\ request -> not (requestInEmptyHouse model.customers request))
        model.requests
  }


updateGameState : Model -> Model
updateGameState model =
  { model
  | state =
    if countLives model <= 0 then
      Stopped
    else
      model.state
  }


incHappinessInTheHouse : House -> Model -> Model
incHappinessInTheHouse house model =
  { model
  | customers = List.map
      (\ customer ->
        if Customer.livesHere house customer then
          Customer.incHappiness customer
        else
          customer
      )
      model.customers
  }


deliverArticle : House -> Article -> Model -> Model
deliverArticle house article model =
  if Request.hasOrder house article.category model.requests then
    { model
    | requests = Request.removeOrders house article.category model.requests
    , articles = model.articles
      |> Article.removeDelivered house article.category
      |> Article.updateState (Delivered house) article
    }
    |> incHappinessInTheHouse house
  else
    model


pickupReturn : House -> House -> Article -> Model -> Model
pickupReturn house articleHouse article model =
  if
    articleHouse == house &&
    List.length (List.filter Article.isPicked model.articles) < model.deliveryPerson.capacity
  then
    { model
    | requests = Request.removeReturns house article model.requests
    , articles = Article.updateState Picked article model.articles
    }
    |> incHappinessInTheHouse house
  else
    model


pickupArticle : Warehouse -> Warehouse -> Article -> Model -> Model
pickupArticle warehouse articleWarehouse article model =
  if warehouse == articleWarehouse &&
    List.length (List.filter Article.isPicked model.articles) < model.deliveryPerson.capacity then
      {model | articles = Article.updateState Picked article model.articles}
  else
    model


returnArticle : Warehouse -> Article -> Model -> Model
returnArticle warehouse article model =
  if List.length (List.filter (Article.inWarehouse warehouse) model.articles) < warehouse.capacity then
    {model | articles = Article.updateState (InStock warehouse) article model.articles}
  else
    model
