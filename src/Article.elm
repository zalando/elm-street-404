module Article (Article, State(..), dispatch, warehouses, house, updateState, removeDelivered, inWarehouse, isPicked, isDelivered, isWorn, availableCategories, return, markInReturn) where
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
    [] -> []
    {state} :: rest ->
      case state of
        InStock warehouse ->
          warehouse :: warehouses rest
        _ -> warehouses rest


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
    [] -> []
    a :: restArticles ->
      if a == article then
        {a | state = state } :: restArticles
      else
        a :: updateState state article restArticles


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


dispatch : Int -> List Warehouse -> Random.Generator (List Article)
dispatch number warehouses =
  if number <= 0 then
    Random.map (always []) (Random.int 0 0)
  else
    IHopeItWorks.pickRandom warehouses
    `Random.andThen`
    (\maybeWarehouse ->
      case maybeWarehouse of
        Just warehouse ->
          Random.map2
            (\category articles -> {category = category, state = InStock warehouse} :: articles)
            Category.random
            (dispatch (number - 1) (IHopeItWorks.remove ((==) warehouse) warehouses))
        Nothing ->
          Random.map (always []) (Random.int 0 0)
    )


return : Int -> List House -> List Article -> Random.Generator (List Article)
return number houses articles =
  if number <= 0 then
    Random.map (always []) (Random.int 0 0)
  else
    let
      deliveredTo = flip isDelivered
      -- keep articles from available slots
      availableArticles = List.filter (\a -> List.any (deliveredTo a) houses) articles
    in
      IHopeItWorks.pickRandom availableArticles
      `Random.andThen`
      (\maybeArticle ->
        case maybeArticle of
          Just article ->
            Random.map
              ((::) article)
              ( return
                  (number - 1)
                  (IHopeItWorks.remove (deliveredTo article) houses)
                  (IHopeItWorks.remove ((==) article) availableArticles)
              )
          Nothing ->
            Random.map (always []) (Random.int 0 0)
      )


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
