module Model (Model, initial, animate, State(..)) where
import Random
import Time exposing (Time)
import AnimationState
import DeliveryPerson exposing (DeliveryPerson)
import Article exposing (Article)
import Request exposing (Request)
import Obstacle exposing (Obstacle)

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
  , obstacles = [Obstacle.fountain (10, 5), Obstacle.fountain (15, 10), Obstacle.tree (2, 3), Obstacle.tree (15, 3)]
  }


animate : Time -> (Time -> Model -> Model) -> Model -> Model
animate time animationFunc model =
  let
    (elapsed, animationState) = AnimationState.animate time model.animationState
  in
    animationFunc elapsed {model | animationState = animationState}
