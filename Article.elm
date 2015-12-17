module Article (Article, State(..), dispatch, updateState, removeDelivered, inWarehouse, isPicked) where
import Random
import House exposing (House)
import Warehouse exposing (Warehouse)
import Category exposing (Category)

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


removeDelivered : House -> Category -> List Article -> List Article
removeDelivered house category articles =
  {- TODO: remove only the first occurence -}
  let
    notInHouse house article =
      not (article.state == Delivered house)
  in
    List.filter (notInHouse house) articles


updateState : State -> Article -> List Article -> List Article
updateState state article articles =
  let
    update article' =
      if article' == article then
        {article' | state = state}
      else
        article'
  in
    List.map update articles


inWarehouse : Warehouse -> Article -> Bool
inWarehouse warehouse article =
  article.state == InStock warehouse


isPicked : Article -> Bool
isPicked {state} = state == Picked


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
