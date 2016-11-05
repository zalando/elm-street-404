module Article
    exposing
        ( Article
        , State(..)
        , dispatch
        , warehouses
        , house
        , updateState
        , removeDelivered
        , inWarehouse
        , isPicked
        , isDelivered
        , availableCategories
        , return
        , markInReturn
        )

import Dict exposing (Dict)
import Random
import MapObject exposing (MapObject, MapObjectCategory(..))
import Customer exposing (Customer)
import Category exposing (Category)
import IHopeItWorks


type State
    = InStock MapObject
    | AwaitingReturn MapObject
    | DeliveredToCustomer Int
    | Picked


type alias Article =
    { category : Category
    , state : State
    }


warehouses : List Article -> List MapObject
warehouses articles =
    case articles of
        [] ->
            []

        { state } :: rest ->
            case state of
                InStock warehouse ->
                    warehouse :: warehouses rest

                _ ->
                    warehouses rest


house : Dict Int Customer -> Article -> Maybe MapObject
house customers { state } =
    case state of
        AwaitingReturn house ->
            Just house

        DeliveredToCustomer id ->
            case Dict.get id customers of
                Just { location } ->
                    location

                Nothing ->
                    Nothing

        _ ->
            Nothing


availableCategories : List Article -> List Category -> List Category
availableCategories articles =
    IHopeItWorks.exclude (List.map .category (List.filter isVacant articles))


removeDelivered : Int -> Category -> List Article -> List Article
removeDelivered customerId category' =
    IHopeItWorks.remove
        (\({ state, category } as article) ->
            state == DeliveredToCustomer customerId && category.kind == category'.kind
        )


updateState : State -> Article -> List Article -> List Article
updateState state article articles =
    case articles of
        [] ->
            []

        a :: restArticles ->
            if a == article then
                { a | state = state } :: restArticles
            else
                a :: updateState state article restArticles


inWarehouse : MapObject -> Article -> Bool
inWarehouse warehouse { state } =
    state == InStock warehouse


isPicked : Article -> Bool
isPicked { state } =
    state == Picked


isDelivered : Dict Int Customer -> MapObject -> Article -> Bool
isDelivered customers house { state } =
    case state of
        DeliveredToCustomer id ->
            case Dict.get id customers of
                Just { location } ->
                    location == Just house

                Nothing ->
                    False

        _ ->
            False


{-| returns true if the article can be ordered
-}
isVacant : Article -> Bool
isVacant { state } =
    case state of
        InStock _ ->
            True

        Picked ->
            True

        _ ->
            False


dispatch : Int -> List MapObject -> Random.Generator (List Article)
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
                                (\category articles -> { category = category, state = InStock warehouse } :: articles)
                                Category.random
                                (dispatch (number - 1) (IHopeItWorks.remove ((==) warehouse) warehouses))

                        Nothing ->
                            Random.map (always []) (Random.int 0 0)
                )


return : Dict Int Customer -> Int -> List MapObject -> List Article -> Random.Generator (List Article)
return customers number houses articles =
    if number <= 0 then
        Random.map (always []) (Random.int 0 0)
    else
        let
            deliveredTo =
                flip (isDelivered customers)

            -- keep articles from available slots
            availableArticles =
                List.filter (\a -> List.any (deliveredTo a) houses) articles
        in
            IHopeItWorks.pickRandom availableArticles
                `Random.andThen`
                    (\maybeArticle ->
                        case maybeArticle of
                            Just article ->
                                Random.map
                                    ((::) article)
                                    (return
                                        customers
                                        (number - 1)
                                        (IHopeItWorks.remove (deliveredTo article) houses)
                                        (IHopeItWorks.remove ((==) article) availableArticles)
                                    )

                            Nothing ->
                                Random.map (always []) (Random.int 0 0)
                    )


markInReturn : Dict Int Customer -> List Article -> List Article -> List Article
markInReturn customers articles articlesToReturn =
    case articles of
        [] ->
            []

        article :: restArticles ->
            if List.member article articlesToReturn then
                let
                    modifiedArticle =
                        case article.state of
                            DeliveredToCustomer id ->
                                case Dict.get id customers of
                                    Just { location } ->
                                        case location of
                                            Just house ->
                                                { article | state = AwaitingReturn house }

                                            _ ->
                                                article

                                    Nothing ->
                                        article

                            _ ->
                                article
                in
                    modifiedArticle :: markInReturn customers restArticles (IHopeItWorks.remove ((==) article) articlesToReturn)
            else
                article :: markInReturn customers restArticles articlesToReturn
