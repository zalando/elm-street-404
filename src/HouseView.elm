module HouseView (render) where

import Actions exposing (Action)
import Layers exposing (layers)
import Box exposing (Box)
import MapObject exposing (MapObject)
import Customer exposing (Customer)
import Request exposing (Request)
import Article exposing (Article)
import RequestView
import CustomerView
import IHopeItWorks
import Textures


getBubbleSprite : Int -> Maybe Textures.TextureId
getBubbleSprite number =
  case number of
    0 -> Nothing
    n -> Just (Textures.HouseBubble n)


render : List Request -> List Article -> List Customer -> MapObject -> List Box
render requests articles customers house =
  let
    requestsFromHouse = List.filter (\r -> r.house == house) requests
    deliveredArticles = List.filter (Article.isDelivered house) articles
    hasRequests = (List.length requestsFromHouse) > 0
    hasArticles = (List.length deliveredArticles) > 0
    renderRequest number =
      RequestView.render
        ( fst house.position - 1
        , snd house.position - toFloat number
        )

    renderBubble =
      case getBubbleSprite (List.length requestsFromHouse) of
        Just sprite ->
          [ Box.textured
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
    [ Box.textured
        Textures.House
        house.position
        0
        (layers.obstacle, 0)
    , Box.textured
        Textures.HouseShadow
        house.position
        0
        (layers.shadow, 0)
    , Box.clickable
        (2, 3)
        (0, -1)
        house.position
        (layers.click, 0)
        (Actions.ClickMapObject house)
    ]
    ++ List.concat (List.indexedMap renderRequest requestsFromHouse)
    ++ renderBubble
    ++ renderCustomer
