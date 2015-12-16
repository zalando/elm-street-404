module Request (..) where

import House exposing (House)
import Article exposing (Article)
import Category exposing (Category)
import Time exposing (Time)


type alias RequestData =
  { elapsed : Time
  }


type Request
  = OrderRequest House Category RequestData
  | ReturnRequest House Article RequestData


category : Request -> Category
category request =
  case request of
    OrderRequest _ category _ -> category
    ReturnRequest _ {category} _ -> category


removeReturns : House -> Article -> List Request -> List Request
removeReturns house article requests =
  {- TODO: remove only the first occurence -}
  let
    notInRequest article request =
      case request of
        ReturnRequest house article _ -> False
        _ -> True
  in
    List.filter (notInRequest article) requests


removeOrders : House -> Category -> List Request -> List Request
removeOrders house category requests =
  {- TODO: remove only the first occurence -}
  let
    notInRequest category request =
      case request of
        OrderRequest house category _ -> False
        _ -> True
  in
    List.filter (notInRequest category) requests


hasOrder : House -> Category -> List Request -> Bool
hasOrder house category requests =
  let
    inRequest category request =
      case request of
        OrderRequest house category _ -> True
        _ -> False
  in
    List.any (inRequest category) requests
