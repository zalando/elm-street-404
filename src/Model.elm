module Model
  ( Model
  , initial
  , start
  , animate
  , navigateToMapObject
  , State(..)
  , timeoutRequests
  , dispatchCustomers
  , updateGameState
  , deliverArticle
  , returnArticle
  , pickupReturn
  , pickupArticle
  , dispatch
  , cleanupLostArticles
  , cleanupLostRequests
  , countLives
  , images
  , resize
  ) where

import Random
import Time exposing (Time)
import AnimationState exposing (Dispatcher, dispatcher, animateDispatcher, dispatcherActions)
import DeliveryPerson exposing (DeliveryPerson)
import Article exposing (Article)
import Request exposing (Request)
import Customer exposing (Customer)
import IHopeItWorks
import Article exposing (Article)
import MapObject exposing (MapObject)


type State = Initialising | Loading | Paused | Playing | Stopped


limitSize : (Int, Int) -> (Int, Int)
limitSize (width, height) =
  ( width |> max 14 |> min 24
  , height |> max 14 |> min 24
  )


gridSize : Int -> (Int, Int) -> (Int, Int)
gridSize tileSize (width, height) =
  limitSize (width // tileSize, height // tileSize)


images : List String
images =
  [ "fountain-spring.png", "house-bubble-2.png", "house.png", "customers.png", "tree.png"
  , "warehouse-shadow.png", "categories.png", "delivery-person.png", "fountain.png"
  , "house-bubble-3.png", "inventory-bubble.png", "shirts.png", "trousers.png"
  , "warehouse.png", "click-to-start.png", "fountain-shadow.png", "house-bubble-1.png"
  , "house-shadow.png", "scarves.png", "shoes.png", "warehouse-bubble.png", "404-elm-street.png"
  ]


type DispatcherAction
  = DispatchArticles Int
  | DispatchOrders Int
  | DispatchReturns Int
  | DispatchCustomers


type alias Model =
  { animationState : AnimationState.AnimationState
  , state : State
  , images : List String
  , seed : Random.Seed
  , tileSize : Int
  , imagesUrl : String
  , dimensions : (Int, Int)
  , gridSize : (Int, Int)
  , deliveryPerson : DeliveryPerson
  , articles : List Article
  , requests : List Request
  , customers : List Customer
  , mapObjects : List MapObject
  , dispatcher : Dispatcher DispatcherAction
  , score : Int
  , maxLives : Int
  }


initial : (Int, Int) -> String -> Model
initial dimensions imagesUrl =
  { animationState = Nothing
  , state = Initialising
  , images = images
  , seed = Random.initialSeed 0
  , tileSize = 40
  , imagesUrl = imagesUrl
  , dimensions = (0, 0)
  , gridSize = (0, 0)
  , deliveryPerson = DeliveryPerson.initial (0, 0)
  , articles = []
  , requests = []
  , mapObjects = []
  , customers = []
  , dispatcher = dispatcher []
  , score = 0
  , maxLives = 3
  }
  |> resize dimensions


resize : (Int, Int) -> Model -> Model
resize dimensions model =
  if model.state == Playing then
    {model | dimensions = dimensions}
  else
    let
      newGridSize = gridSize model.tileSize dimensions
    in
      { model
      | dimensions = dimensions
      , gridSize = newGridSize
      , deliveryPerson =
          DeliveryPerson.initial
            ( toFloat (fst newGridSize // 2 - 1)
            , toFloat (snd newGridSize // 4 * 3 - 1)
            )
      , articles = []
      , requests = []
      , mapObjects = []
      , customers = []
      , dispatcher = dispatcher []
      }


positionObstacles : Model -> Model
positionObstacles ({gridSize, deliveryPerson} as model) =
  let
    (width, height) = gridSize
    boxes = MapObject.splitBy
      { size = model.deliveryPerson.size
      , position = model.deliveryPerson.position
      }
      { size = (toFloat width - 2, toFloat height - 6)
      , position = (1, 4)
      }
    (mapObjects, seed) =
      Random.generate
        ( MapObject.placeRandom
            ( List.map (always MapObject.warehouse) [0..1] ++
              List.map (always MapObject.house) [0..3] ++
              (MapObject.fountain :: List.map (always MapObject.tree) [0..3])
            )
            boxes
        )
        model.seed
  in
    { model
    | mapObjects = mapObjects
    , seed = seed
    }


start : Model -> Model
start model =
  { model
  | state = Playing
  , articles = []
  , requests = []
  , customers = []
  , dispatcher =
      dispatcher
        [ (11000, DispatchOrders 1)
        , (13000, DispatchArticles 1)
        , (31000, DispatchReturns 1)
        , (5000, DispatchCustomers)
        ]
  , score = 0
  , maxLives = 3
  }
  |> resize model.dimensions
  |> positionObstacles
  |> dispatchCustomers
  |> dispatchArticles 6
  |> dispatchOrders 3


dispatch : Time -> Model -> Model
dispatch elapsed model =
  let
    dispatcher = animateDispatcher elapsed model.dispatcher
  in
    List.foldl dispatchAction {model | dispatcher = dispatcher} (dispatcherActions dispatcher)


dispatchAction : DispatcherAction -> Model -> Model
dispatchAction action =
  case action of
    DispatchArticles n -> dispatchArticles n
    DispatchOrders n -> dispatchOrders n
    DispatchReturns n -> dispatchReturns n
    DispatchCustomers -> dispatchCustomers


dispatchArticles : Int -> Model -> Model
dispatchArticles number model =
  let
    initialSlots = IHopeItWorks.exclude
      (MapObject.warehouseSlots model.mapObjects)
      (Article.warehouses model.articles)
    (articles, seed) = Random.generate (Article.dispatch number initialSlots) model.seed
  in
    { model | articles = model.articles ++ articles, seed = seed }


dispatchOrders : Int -> Model -> Model
dispatchOrders number model =
  let
    categories = Article.availableCategories
      model.articles
      (Request.orderedCategories model.requests)
    slots = IHopeItWorks.exclude
      (MapObject.houseSlots model.mapObjects)
      (List.map .house model.requests)
    (orders, seed) = Random.generate (Request.orders number slots categories) model.seed
  in
    { model | requests = model.requests ++ orders, seed = seed }


dispatchReturns : Int -> Model -> Model
dispatchReturns number model =
  let
    housesWithArticles = List.filter
      (\h -> List.any (Article.isDelivered h) model.articles)
      model.mapObjects
    slots = IHopeItWorks.exclude
      (MapObject.houseSlots housesWithArticles)
      (List.map .house model.requests)
    (articlesToReturn, seed) = Random.generate (Article.return number slots model.articles) model.seed
    articles = Article.markInReturn model.articles articlesToReturn
    returnedArticles = Article.markInReturn articlesToReturn articlesToReturn
    returns = Request.returnArticles returnedArticles
  in
    { model
    | articles = articles
    , requests = model.requests ++ returns
    , seed = seed
    }


obstacleTiles : List MapObject -> List (Int, Int)
obstacleTiles  =
  let
    col y h x = (++) (List.map ((,) x) [y..y + h - 1])
    cols (x, y) (w, h) = (++) (List.foldl (col y h) [] [x..x + w - 1])
    toIntTuple (a, b) = (round a, round b)
  in
    List.foldl (\{position, size} -> cols (toIntTuple position) (toIntTuple size)) []


placeToLocation : MapObject -> (Int, Int)
placeToLocation {position, size} =
  ( round (fst position + snd size / 2 - 1)
  , round (snd position + snd size)
  )


navigateToMapObject : MapObject -> Model -> Model
navigateToMapObject mapObject =
  navigateTo
    (DeliveryPerson.OnTheWayTo mapObject)
    (placeToLocation mapObject)


navigateTo : DeliveryPerson.Location -> (Int, Int) -> Model -> Model
navigateTo location destination model =
  { model
  | deliveryPerson = DeliveryPerson.navigateTo
      model.gridSize
      (obstacleTiles model.mapObjects)
      location
      destination
      model.deliveryPerson
  }


animate : Time -> (Time -> Model -> Model) -> Model -> Model
animate time animationFunc model =
  let
    (elapsed, animationState) = AnimationState.animate time model.animationState
  in
    animationFunc elapsed {model | animationState = animationState}


decHappinessIfHome : List MapObject -> Customer -> Customer
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
    (List.map .house timeouted)
    customer


countLives: Model -> Int
countLives model =
  model.maxLives -
  ( model.customers
    |> List.filter Customer.isLost
    |> List.length
  )


timeoutRequests : Model -> Model
timeoutRequests model =
  let
    (inTime, timeouted) = List.partition Request.inTime model.requests
  in
    { model
    | requests = inTime
    , customers = List.map (decHappiness timeouted) model.customers
    }


houseEmpty : List Customer -> MapObject -> Bool
houseEmpty customers house =
  case customers of
    [] -> True
    customer :: otherCustomers ->
      if Customer.livesHere house customer then
        False
      else
        houseEmpty otherCustomers house


dispatchCustomers : Model -> Model
dispatchCustomers model =
  let
    emptyHouses = List.filter (houseEmpty model.customers) model.mapObjects
    (newCustomers, seed) = Random.generate (Customer.rodnams emptyHouses) model.seed
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
        (\ request -> not (houseEmpty model.customers request.house))
        model.requests
  }


updateGameState : Model -> Model
updateGameState model =
  if countLives model <= 0 then
    { model | state = Stopped }
  else
    model


incHappinessInTheHouse : MapObject -> Model -> Model
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


deliverArticle : MapObject -> Article -> Model -> Model
deliverArticle house article model =
  if Request.hasOrder house article.category model.requests then
    { model
    | requests = IHopeItWorks.remove
        (Request.isOrdered house article.category)
        model.requests
    , articles = model.articles
      |> Article.removeDelivered house article.category
      |> Article.updateState (Article.Delivered house) article
    , score = model.score + 1
    }
    |> incHappinessInTheHouse house
  else
    model


pickupReturn : MapObject -> MapObject -> Article -> Model -> Model
pickupReturn house articleHouse article model =
  if
    articleHouse == house &&
    List.length (List.filter Article.isPicked model.articles) < model.deliveryPerson.capacity
  then
    { model
    | requests = IHopeItWorks.remove (Request.isInReturn house article) model.requests
    , articles = Article.updateState Article.Picked article model.articles
    , score = model.score + 1
    }
    |> incHappinessInTheHouse house
  else
    model


pickupArticle : MapObject -> MapObject -> Article -> Model -> Model
pickupArticle warehouse articleWarehouse article model =
  if warehouse == articleWarehouse &&
    List.length (List.filter Article.isPicked model.articles) < model.deliveryPerson.capacity then
      {model | articles = Article.updateState Article.Picked article model.articles}
  else
    model


returnArticle : MapObject -> Article -> Model -> Model
returnArticle warehouse article model =
  case warehouse.category of
    MapObject.WarehouseCategory capacity ->
      if List.length (List.filter (Article.inWarehouse warehouse) model.articles) < capacity then
        {model | articles = Article.updateState (Article.InStock warehouse) article model.articles}
      else
        model
    _ ->
       model
