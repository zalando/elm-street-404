module Article (Article, State(..), dispatch, warehouses, updateState, removeDelivered, inWarehouse, isPicked, isDelivered, availableCategories) where
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
  }


warehouses : List Article -> List Warehouse
warehouses articles =
  case articles of
    article :: rest ->
      case article.state of
        InStock warehouse ->
          warehouse :: warehouses rest
        _ -> warehouses rest
    _ -> []


availableCategories : List Article -> List Category -> List Category
availableCategories articles =
  IHopeItWorks.exclude (List.map .category (List.filter isVacant articles))


removeDelivered : House -> Category -> List Article -> List Article
removeDelivered house category =
  IHopeItWorks.remove (\{state} -> state == Delivered house)


updateState : State -> Article -> List Article -> List Article
updateState state article articles =
  case articles of
    a :: restArticles ->
      if a == article then
        {a | state = state } :: restArticles
      else
        a :: updateState state article restArticles
    [] -> []


inWarehouse : Warehouse -> Article -> Bool
inWarehouse warehouse article =
  article.state == InStock warehouse


isPicked : Article -> Bool
isPicked {state} = state == Picked


isDelivered : House -> Article -> Bool
isDelivered house {state} =
  case state of
    Delivered house' -> house == house'
    _ -> False


{- returns true if the article can be ordered -}
isVacant : Article -> Bool
isVacant {state} =
  case state of
    InStock _ -> True
    AwaitingReturn _ -> True
    Picked -> True
    _ -> False


dispatch : Int -> List Warehouse -> Random.Seed -> (List Article, Random.Seed)
dispatch number warehouses seed =
  if number == 0 then
    ([], seed)
  else
    case IHopeItWorks.pickRandom warehouses seed of
      (Just warehouse, seed') ->
        let
          restWarehouses = IHopeItWorks.remove ((==) warehouse) warehouses
          (category, seed'') = Category.random seed'
          (items, seed''') = dispatch (number - 1) restWarehouses seed''
        in
          ( { category = category
            , state = InStock warehouse
            }
            :: items
          , seed'''
          )
      (Nothing, seed') ->
        ([], seed')
