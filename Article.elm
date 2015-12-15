module Article (Article, State(..), Category(..), dispatch, updateState, removeDelivered, filterInWarehouse) where
import Random
import Array
import House exposing (House)
import Warehouse exposing (Warehouse)


type State
  = InStock Warehouse
  | AwaitingReturn House
  | Delivered House
  | Picked


type Category
  = Pants Int
  | Shirt Int
  | Shoes Int
  | Scarf Int


type alias Article =
  { category : Category
  , state : State
  , id : Random.Seed
  }


categories : Array.Array (Int -> Category)
categories = Array.fromList [Pants, Shirt, Shoes, Scarf]


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


filterInWarehouse : Warehouse -> List Article -> List Article
filterInWarehouse warehouse articles =
  List.filter (inWarehouse warehouse) articles


filterInDelivery : List Article -> List Article
filterInDelivery articles =
  List.filter inDelivery articles


inWarehouse : Warehouse -> Article -> Bool
inWarehouse warehouse article =
  article.state == InStock warehouse


inDelivery : Article -> Bool
inDelivery article =
  article.state == Picked


dispatch : Warehouse -> Random.Seed -> (Article, Random.Seed)
dispatch warehouse seed =
  let
    (color, seed') = Random.generate (Random.int 0 3) seed
    (categoryIndex, seed'') = Random.generate (Random.int 0 (Array.length categories)) seed'
    category = (Maybe.withDefault Scarf (Array.get categoryIndex categories)) color
  in
    ({category = category, state = InStock warehouse, id = seed}, seed'')
