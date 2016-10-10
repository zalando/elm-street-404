module Model exposing
  ( Model
  , initial
  , start
  , animate
  , navigateToMapObject
  , State(..)
  , deliverArticle
  , returnArticle
  , pickupReturn
  , pickupArticle
  , resize
  , render
  , click
  , dispatch
  )

import Random
import Time exposing (Time)
import DeliveryPerson exposing (DeliveryPerson)
import Article exposing (Article)
import Request exposing (Request)
import Customer exposing (Customer, Location(..))
import IHopeItWorks
import Article exposing (Article)
import MapObject exposing (MapObject, MapObjectCategory(..))
import Textures exposing (TextureId, Textures)
import Box exposing (Box, ClickableBoxData, TexturedBoxData)
import Actions exposing (Action, EventAction(..))
-- Views:
import TreeView
import FountainView
import HouseView
import CustomerView
import WarehouseView
import DeliveryPersonView
import InventoryView
import Article
import ScoreView
import StartGameView
import DigitsView


type State
  = Initialising
  | Loading
  | Paused
  | Playing
  | Stopped
  | Suspended State -- to store the prev state


minMapWidth : Int
minMapWidth = 16


minMapHeight : Int
minMapHeight = 16


maxMapWidth : Int
maxMapWidth = 24


maxMapHeight : Int
maxMapHeight = 24


limitSize : (Int, Int) -> (Int, Int)
limitSize (width, height) =
  ( width |> max minMapWidth |> min maxMapWidth
  , height |> max minMapHeight |> min maxMapHeight
  )


gridSize : Int -> (Int, Int) -> (Int, Int)
gridSize tileSize (width, height) =
  limitSize (width // tileSize, height // tileSize)


type alias Model =
  { state : State
  , textures : Textures
  , devicePixelRatio : Float
  , seed : Random.Seed
  , tileSize : Int
  , imagesUrl : String
  , embed : Bool
  , dimensions : (Int, Int)
  , gridSize : (Int, Int)
  , deliveryPerson : DeliveryPerson
  , articles : List Article
  , requests : List Request
  , customers : List Customer
  , mapObjects : List MapObject
  , events : List (Time, EventAction)
  , score : Int
  , maxLives : Int
  , clickableBoxes : List ClickableBoxData
  , texturedBoxes : List TexturedBoxData
  , closeButtonActive : Bool
  }


initial : Int -> String -> Bool -> Float -> Model
initial randomSeed imagesUrl embed devicePixelRatio =
  { state = Initialising
  , embed = embed
  , devicePixelRatio = devicePixelRatio
  , textures = Textures.textures
  , seed = Random.initialSeed randomSeed
  , tileSize = 0
  , imagesUrl = imagesUrl
  , dimensions = (0, 0)
  , gridSize = (0, 0)
  , deliveryPerson = DeliveryPerson.initial (0, 0)
  , articles = []
  , requests = []
  , mapObjects = []
  , customers = []
  , events = []
  , score = 0
  , maxLives = 3
  , clickableBoxes = []
  , texturedBoxes = []
  , closeButtonActive = False
  }


resize : (Int, Int) -> Model -> Model
resize dimensions model =
  if model.state == Playing then
    {model | dimensions = dimensions}
  else
    let
      newTileSize = min (fst dimensions // minMapWidth) 40
      newGridSize = gridSize newTileSize dimensions
    in
      { model
      | dimensions = dimensions
      , gridSize = newGridSize
      , tileSize = newTileSize
      , deliveryPerson =
          DeliveryPerson.initial
            ( toFloat (fst newGridSize // 2 - 1)
            , toFloat (snd newGridSize // 4 * 3)
            )
      , articles = []
      , requests = []
      , mapObjects = []
      , customers = []
      , events = []
      }


positionObstacles : Model -> Model
positionObstacles ({gridSize, deliveryPerson} as model) =
  let
    (width, height) = gridSize
    boxes = MapObject.splitBy
      { size = model.deliveryPerson.size
      , position = model.deliveryPerson.position
      }
      { size = (toFloat width - 3, toFloat height - 6)
      , position = (2, 4)
      }
    (mapObjects, seed) =
      Random.step
        ( MapObject.placeRandom
            ( List.repeat 2 MapObject.warehouse ++
              List.repeat 4 MapObject.house ++
              MapObject.fountain ::
              List.repeat 4 MapObject.tree
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
  , events =
      [ (11000, DispatchOrders 1)
      , (13000, DispatchArticles 1)
      , (31000, DispatchReturns 1)
      , (5000, DispatchCustomers)
      , (1000, TimeoutRequestsAndCleanup)
      ]
  , score = 0
  , maxLives = 3
  }
  |> resize model.dimensions
  |> positionObstacles
  |> dispatchCustomers
  |> dispatchArticles 6
  |> dispatchOrders 3


animate : Time -> Model -> (Model, Maybe Action)
animate time =
  animationLoop (min time 25)


animationLoop : Time -> Model -> (Model, Maybe Action)
animationLoop elapsed model =
  let
    (deliveryPerson, maybeAction) = DeliveryPerson.animate elapsed model.deliveryPerson
  in
    ( render
        { model
        | mapObjects = List.map (MapObject.animate elapsed) model.mapObjects
        , deliveryPerson = deliveryPerson
        , requests = List.map (Request.animate elapsed) model.requests
        , customers = List.map (Customer.animate elapsed) model.customers
        }
    , maybeAction
    )


dispatch : EventAction -> Model -> Model
dispatch action =
  case action of
    DispatchArticles n -> dispatchArticles n
    DispatchOrders n -> dispatchOrders n
    DispatchReturns n -> dispatchReturns n
    DispatchCustomers -> dispatchCustomers
    TimeoutRequestsAndCleanup -> timeoutRequests >> cleanup >> updateGameState


dispatchArticles : Int -> Model -> Model
dispatchArticles number model =
  let
    initialSlots = IHopeItWorks.exclude
      (MapObject.warehouseSlots model.mapObjects)
      (Article.warehouses model.articles)
    (articles, seed) = Random.step (Article.dispatch number initialSlots) model.seed
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
    (orders, seed) = Random.step (Request.orders number slots categories) model.seed
  in
    { model | requests = model.requests ++ orders, seed = seed }


dispatchReturns : Int -> Model -> Model
dispatchReturns number model =
  let
    -- if the only article in the house was returned,
    -- then the customer would dissapear
    housesWithMoreThanOneArticle = List.filter
      (\h -> List.length (List.filter (Article.isDelivered h) model.articles) > 1)
      model.mapObjects
    slots = IHopeItWorks.exclude
      (MapObject.houseSlots housesWithMoreThanOneArticle)
      (List.map .house model.requests)
    (articlesToReturn, seed) = Random.step (Article.return number slots model.articles) model.seed
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
    -- add an additional col of tiles at x - 1, because delivery person's width = 2 tiles
    cols (x, y) (w, h) = (++) (List.foldl (col y h) [] [x - 1..x + w - 1])
    toIntTuple (a, b) = (round a, round b)
  in
    List.foldl (\{position, size} -> cols (toIntTuple position) (toIntTuple size)) []


placeToLocation : MapObject -> (Int, Int)
placeToLocation {position, size} =
  ( round (fst position + fst size / 2 - 1)
  , round (snd position + snd size)
  )


navigateToMapObject : MapObject -> Maybe Action -> Model -> Model
navigateToMapObject mapObject maybeAction model =
  { model
  | deliveryPerson = DeliveryPerson.navigateTo
      model.gridSize
      (obstacleTiles model.mapObjects)
      (DeliveryPerson.OnTheWayTo mapObject maybeAction)
      (placeToLocation mapObject)
      model.deliveryPerson
  }


decHappinessIfHome : List Request -> Customer -> Customer
decHappinessIfHome requests customer =
  case requests of
    [] -> customer
    {house} :: rest ->
      if Customer.livesHere house customer then
        Customer.decHappiness customer
      else
        decHappinessIfHome rest customer


countLives : Model -> Int
countLives {maxLives, customers} =
  maxLives - List.length (List.filter Customer.isLost customers)


timeoutRequests : Model -> Model
timeoutRequests model =
  let
    (inTime, timeouted) = List.partition Request.inTime model.requests
  in
    { model
    | requests = inTime
    , customers = List.map (decHappinessIfHome timeouted) model.customers
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
    emptyHouses = model.mapObjects
      |> List.filter MapObject.isHouse
      |> List.filter (houseEmpty model.customers)
    (newCustomers, seed) = Random.step (Customer.rodnams emptyHouses) model.seed
  in
    { model
    | customers = model.customers ++ newCustomers
    , seed = seed
    }


cleanup : Model -> Model
cleanup model =
  { model
  | requests =
      List.filter
        (\ request -> not (houseEmpty model.customers request.house))
        model.requests
  }


updateGameState : Model -> Model
updateGameState model =
  if countLives model <= 0 then
    render { model | state = Stopped }
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
  if List.any (Request.isOrdered house article.category) model.requests then
    { model
    | requests = IHopeItWorks.remove
        (Request.isOrdered house article.category)
        model.requests
    , articles = model.articles
      |> Article.removeDelivered house article.category
      |> (
        case IHopeItWorks.find (Customer.livesHere house) model.customers of
          Just customer ->
            Article.updateState (Article.Delivered customer) article
          Nothing ->
            identity
      )
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


renderCustomer : Model -> Customer -> List Box
renderCustomer model customer =
  case customer.location of
    Customer.AtHome {position} ->
      CustomerView.render model.articles position customer
    Customer.Lost ->
      []


renderMapObject : Model -> MapObject -> List Box
renderMapObject model mapObject =
  case mapObject.category of
    TreeCategory ->
      TreeView.render mapObject
    FountainCategory fountain ->
      FountainView.render fountain mapObject
    HouseCategory _ ->
      HouseView.render model.requests model.articles model.customers mapObject
    WarehouseCategory capacity ->
      WarehouseView.render model.articles capacity mapObject


click : (Int, Int) -> Model -> Maybe Action
click coordinates model =
  model.clickableBoxes
    |> List.filter (Box.clicked (clickToTile model coordinates))
    |> List.sortBy (.layer >> negate)
    |> List.head
    |> Maybe.map .onClickAction


clickToTile : Model -> (Int, Int) -> (Float, Float)
clickToTile {tileSize} (x, y) =
  ( toFloat x / toFloat tileSize
  , toFloat y / toFloat tileSize
  )


render : Model -> Model
render model =
  let
    (texturedBoxes, clickableBoxes) = Box.split (boxes model)
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
      DigitsView.render
        (toFloat (fst model.gridSize) / 2 + 1, toFloat (snd model.gridSize) / 2)
        (Textures.loadedTextures model.textures)
    _ ->
      InventoryView.render model.gridSize model.articles
      ++ DeliveryPersonView.render (List.length (List.filter Article.isPicked model.articles)) model.deliveryPerson
      ++ ScoreView.render model.gridSize model.score model.maxLives (countLives model)
      ++ if model.state == Stopped then StartGameView.render model.gridSize else []
      ++ List.concatMap (renderMapObject model) model.mapObjects
      ++ List.concatMap (renderCustomer model) model.customers
