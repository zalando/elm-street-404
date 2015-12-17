module Request (Request(..), removeOrders, removeReturns, inHouse, hasOrder, animate) where

import House exposing (House)
import Article exposing (Article)
import Category exposing (Category)
import Time exposing (Time)


maxWaitingTime : Time
maxWaitingTime = 15000


type alias RequestData =
  { elapsed : Time
  , blinkHidden : Bool
  }


type Request
  = Order House Category RequestData
  | Return House Article RequestData


inHouse : House -> Request -> Bool
inHouse house request =
  case request of
    Order house'' _ _ -> house'' == house
    Return house'' _ _ -> house'' == house


category : Request -> Category
category request =
  case request of
    Order _ category _ -> category
    Return _ {category} _ -> category


removeReturns : House -> Article -> List Request -> List Request
removeReturns house article requests =
  {- TODO: remove only the first occurence -}
  List.filter (\r -> not (isInReturn house article r)) requests


isInReturn : House -> Article -> Request -> Bool
isInReturn house article request =
  case request of
    Return house' article' _ ->
      house' == house && article' == article
    _ ->
      False


isOrdered : House -> Category -> Request -> Bool
isOrdered house category request =
  case request of
    Order house' category' _ ->
      house' == house && category' == category
    _ -> False


removeOrders : House -> Category -> List Request -> List Request
removeOrders house category requests =
  {- TODO: remove only the first occurence -}
  List.filter (\r -> not (isOrdered house category r)) requests


hasOrder : House -> Category -> List Request -> Bool
hasOrder house category requests =
  List.any (isOrdered house category) requests


a : Float
a = 1

b : Float
b = 0

c : Float
c = 0


flash : Time -> Bool
flash elapsed = 0 < sin (a * (elapsed ^ 2) + b * elapsed + c)


animateRequestData : Time -> RequestData -> RequestData
animateRequestData time request =
  { request
  | elapsed = request.elapsed + time
  -- , blinkHidden = Debug.log "hidden" (flash request.elapsed)
  , blinkHidden = flash request.elapsed
  }


animate : Time -> Request -> Request
animate time request =
  case request of
    Order house category data -> Order house category (animateRequestData time data)
    Return house article data -> Return house article (animateRequestData time data)
