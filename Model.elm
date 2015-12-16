module Model (Model, initial, animate, navigateTo, State(..)) where
import Random
import Time exposing (Time)
import AnimationState
import DeliveryPerson exposing (DeliveryPerson)
import Article exposing (Article)
import Request exposing (Request)
import Obstacle exposing (Obstacle)
import House exposing (House)
import Warehouse exposing (Warehouse)

type State = Paused | Playing | Stopped

type alias Model =
  { animationState : AnimationState.AnimationState
  , state : State
  , seed : Random.Seed
  , tileSize : Int
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
  , deliveryPerson = DeliveryPerson.initial (10, 10)
  , articles = []
  , requests = []
  , obstacles = [ Obstacle.fountain (10, 5)
                , Obstacle.fountain (20, 1)
                , Obstacle.tree (1, 5)
                , Obstacle.tree (15, 3)
                ]
  , houses = [ House.house (8, 10)
             , House.house (12, 7)
             , House.house (4, 3)
             ]
  , warehouses = [ Warehouse.warehouse (19, 4)
                 , Warehouse.warehouse (1, 10)
                 ]
  }


navigateTo : (Int, Int) -> Model -> Model
navigateTo destination model =
  model


animate : Time -> (Time -> Model -> Model) -> Model -> Model
animate time animationFunc model =
  let
    (elapsed, animationState) = AnimationState.animate time model.animationState
  in
    animationFunc elapsed {model | animationState = animationState}
