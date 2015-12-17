module Request (Request(..), removeOrders, removeReturns, inHouse, hasOrder, animate, initData) where

import House exposing (House)
import Article exposing (Article)
import Category exposing (Category)
import Time exposing (Time)


maxWaitingTime : Time
maxWaitingTime = 60000


type alias RequestData =
  { timeout : Time
  , elapsed : Time
  , blinkHidden : Bool
  }


type Request
  = Order House Category RequestData
  | Return House Article RequestData


initData : RequestData
initData =
 { timeout = maxWaitingTime
 , elapsed = 0
 , blinkHidden = False
 }


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


-- time while it doesn't blink
z : Float
z = 30000


-- acceleration of blinking speed
a : Float
a = 0.000000024

-- initial blinking speed
b : Float
b = 0.003

-- constant time shift (positive to make sure it starts with not blinking)
c : Float
c = 0


-- max speed
m : Float
m = 0.015


flash : Time -> Bool
flash elapsed =
  if elapsed < z then
    False
  else
    let
      x = elapsed - z
      s = if a * x + b > m then m else a * x + b
    in
     0 < sin (s * x + c) -- ((a * x * x) + (b * x) + c)


animateRequestData : Time -> RequestData -> RequestData
animateRequestData time request =
  { request
  | elapsed = request.elapsed + time
  , blinkHidden = flash request.elapsed
  }


animate : Time -> Request -> Request
animate time request =
  case request of
    Order house category data -> Order house category (animateRequestData time data)
    Return house article data -> Return house article (animateRequestData time data)
