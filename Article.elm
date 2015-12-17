module Article (Article, State(..), dispatch, updateState, removeDelivered, inWarehouse, isPicked, availableCategories) where
import Random
import House exposing (House)
import Warehouse exposing (Warehouse)
import Category exposing (Category)
import IHopeItWorks

type State
  = InStock Warehouse
  | AwaitingReturn House
  | Delivered House
  | Picked


type alias Article =
  { category : Category
  , state : State
  , id : Random.Seed
  }


availableCategories : List Article -> List Category -> List Category
availableCategories articles =
  IHopeItWorks.exclude (List.map .category (List.filter isVacant articles))


removeDelivered : House -> Category -> List Article -> List Article
removeDelivered house category =
  {- TODO: remove only the first occurence -}
  let
    notInHouse house article =
      not (article.state == Delivered house)
  in
    List.filter (notInHouse house)


updateState : State -> Article -> List Article -> List Article
updateState state article =
  let
    update article' =
      if article' == article then
        {article' | state = state}
      else
        article'
  in
    List.map update


inWarehouse : Warehouse -> Article -> Bool
inWarehouse warehouse article =
  article.state == InStock warehouse


isPicked : Article -> Bool
isPicked {state} = state == Picked


{- returns true if the article can be ordered -}
isVacant : Article -> Bool
isVacant {state} =
  case state of
    InStock _ -> True
    AwaitingReturn _ -> True
    Picked -> True
    _ -> False


dispatch : Int -> Warehouse -> Random.Seed -> (List Article, Random.Seed)
dispatch number warehouse seed =
  if number == 0 then
    ([], seed)
  else
    let
      (category, seed') = Category.random seed
      (items, seed'') = dispatch (number - 1) warehouse seed'
    in
      ( { category = category
        , state = InStock warehouse
        , id = seed'
        }
        :: items
      , seed''
      )
