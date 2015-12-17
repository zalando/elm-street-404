module Request (Request(..), removeOrders, removeReturns, category, inHouse, hasOrder) where

import House exposing (House)
import Article exposing (Article)
import Category exposing (Category)
import Time exposing (Time)


type alias RequestData =
  { elapsed : Time
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
