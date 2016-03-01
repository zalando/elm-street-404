module HouseView (render) where

import Actions exposing (Action)
import Html.Events exposing (onClick)
import Layers exposing (layers)
import Sprite
import MapObject exposing (MapObject)
import Customer exposing (Customer)
import Request exposing (Request)
import Article exposing (Article)
import RequestView
import CustomerView
import IHopeItWorks


getBubbleSprite : Int -> Maybe Sprite.TextureId
getBubbleSprite number =
  case number of
    0 -> Nothing
    n -> Just (Sprite.HouseBubble n)


render : Signal.Address Action -> List Request -> List Article -> List Customer -> MapObject -> List Sprite.Box
render address requests articles customers house =
  let
    requestsFromHouse = List.filter (\r -> r.house == house) requests
    deliveredArticles = List.filter (Article.isDelivered house) articles
    hasRequests = (List.length requestsFromHouse) > 0
    hasArticles = (List.length deliveredArticles) > 0
    renderRequest number =
      RequestView.render
        address
        ( fst house.position - 1
        , snd house.position - toFloat number
        )

    renderBubble =
      case getBubbleSprite (List.length requestsFromHouse) of
        Just sprite ->
          [ Sprite.box
              sprite
              house.position
              0
              (layers.bubble, 0)
          ]
        _ -> []
    renderCustomer =
      case IHopeItWorks.find (Customer.livesHere house) customers of
        Nothing -> []
        Just customer ->
          if customer.happiness == 2 && not hasRequests && not hasArticles then
            []
          else
            CustomerView.render requestsFromHouse deliveredArticles house customer
  in
    [ Sprite.box
        Sprite.House
        house.position
        0
        (layers.obstacle, 0)
    , Sprite.box
        Sprite.HouseShadow
        house.position
        0
        (layers.shadow, 0)
    , Sprite.empty
        (2, 3)
        (0, -1)
        house.position
        (layers.clickAbove, 0)
        [onClick address (Actions.ClickMapObject house)]
    ]
    ++ List.concat (List.indexedMap renderRequest requestsFromHouse)
    ++ renderBubble
    ++ renderCustomer
