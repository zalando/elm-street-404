module Article (Article, State(..), dispatch, warehouses, house, updateState, removeDelivered, inWarehouse, isPicked, isDelivered, isWorn, availableCategories, chooseToReturn, markInReturn) where
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


house : Article -> Maybe House
house {state} =
  case state of
    AwaitingReturn house -> Just house
    Delivered house -> Just house
    _ -> Nothing


availableCategories : List Article -> List Category -> List Category
availableCategories articles =
  IHopeItWorks.exclude (List.map .category (List.filter isVacant articles))


removeDelivered : House -> Category -> List Article -> List Article
removeDelivered house category' =
  IHopeItWorks.remove (\{state, category} -> state == Delivered house && Category.isSame category category')


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


isWorn : Article -> Bool
isWorn {state} =
  case state of
    Delivered _ -> True
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


chooseToReturn : Int -> List House -> List Article -> Random.Seed -> (List Article, Random.Seed)
chooseToReturn number houses articles seed =
  if number == 0 then
    ([], seed)
  else
    let
      deliveredTo article house = isDelivered house article
      -- keep articles from available slots
      availableArticles = List.filter (\a -> List.any (deliveredTo a) houses) articles
    in
      case IHopeItWorks.pickRandom availableArticles seed of
        (Just article, seed') ->
          let
            restArticles = IHopeItWorks.remove ((==) article) availableArticles
            restHouses = IHopeItWorks.remove (deliveredTo article) houses
            (returnArticles, seed'') = chooseToReturn (number - 1) restHouses restArticles seed'
          in
            (article :: returnArticles, seed'')
        (Nothing, seed') ->
          ([], seed')


markInReturn : List Article -> List Article -> List Article
markInReturn articles articlesToReturn =
  case articles of
    [] -> []
    article :: restArticles ->
      if List.member article articlesToReturn then
        let
          modifiedArticle = case article.state of
            Delivered house -> {article | state = AwaitingReturn house}
            _ -> article
        in
          modifiedArticle :: markInReturn restArticles (IHopeItWorks.remove ((==) article) articlesToReturn)
      else
        article :: markInReturn restArticles articlesToReturn
