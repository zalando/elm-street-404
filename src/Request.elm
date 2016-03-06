module Request (Request, RequestCategory(..), isInReturn, isOrdered, animate, inTime, orders, orderedCategories, returnArticles) where

import MapObject exposing (MapObject)
import Article exposing (Article)
import Category exposing (Category)
import Time exposing (Time)
import IHopeItWorks
import Random
import AnimationState exposing (AnimatedObject)


type RequestCategory
  = Order Category
  | Return Article


type alias Request =
  AnimatedObject
    { house : MapObject
    , category : RequestCategory
    , blinkHidden : Bool
    }


request : RequestCategory -> MapObject -> Request
request category house =
  { timeout = 60000
  , elapsed = 0
  , blinkHidden = False
  , house = house
  , category = category
  }


orderedCategories : List Request -> List Category
orderedCategories requests =
  case requests of
    [] -> []
    request :: rest ->
      case request.category of
        Order category ->
          category :: orderedCategories rest
        _ ->
          orderedCategories rest


returnArticles : List Article -> List Request
returnArticles articles =
  case articles of
    [] -> []
    article :: restArticles ->
      case Article.house article of
        Just house ->
          request (Return article) house :: returnArticles restArticles
        Nothing ->
          returnArticles restArticles


orders : Int -> List MapObject -> List Category -> Random.Generator (List Request)
orders number houses categories =
  if number <= 0 then
    Random.map (always []) (Random.int 0 0)
  else
    Random.pair (IHopeItWorks.pickRandom houses) (IHopeItWorks.pickRandom categories)
    `Random.andThen`
    (\pair ->
      case pair of
        (Just house, Just category) ->
          Random.map
            ((::) (request (Order category) house))
            ( orders
                (number - 1)
                (IHopeItWorks.remove ((==) house) houses)
                (IHopeItWorks.remove ((==) category) categories)
            )
        _ ->
          Random.map (always []) (Random.int 0 0)
    )


isInReturn : MapObject -> Article -> Request -> Bool
isInReturn house article request =
  case request.category of
    Return article' ->
      house == request.house && article' == article
    _ ->
      False


isOrdered : MapObject -> Category -> Request -> Bool
isOrdered house category request =
  case request.category of
    Order category' ->
      house == request.house && category' == category
    _ -> False


flash : Time -> Bool
flash elapsed =
  let
    z = 30000 -- time while it doesn't blink
    a = 0.00000024 -- acceleration of blinking speed
    b = 0.003 -- initial blinking speed
    m = 0.015 -- max speed
  in
    if elapsed < z then
      False
    else
      let
        x = elapsed - z
        s = if a * x + b > m then m else a * x + b
      in
        0 < sin (s * x)


animate : Time -> Request -> Request
animate time request =
  { request
  | elapsed = request.elapsed + time
  , blinkHidden = flash request.elapsed
  }


inTime : Request -> Bool
inTime {elapsed, timeout} =
  elapsed < timeout
