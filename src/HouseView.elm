module HouseView exposing (render)

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


render : List Request -> List Article -> List Customer -> MapObject -> List Box
render requests articles customers ({position} as house) =
  let
    requestsFromHouse = List.filter (\r -> r.house == house) requests
    deliveredArticles = List.filter (Article.isDelivered house) articles
    hasRequests = List.length requestsFromHouse > 0
    hasArticles = List.length deliveredArticles > 0

    renderRequest number =
      RequestView.render (fst position - 1, snd position - toFloat number)

    renderBubble =
      case List.length requestsFromHouse of
        0 ->
          []
        n ->
          [Box.offsetTextured (-2, toFloat -n) (Textures.HouseBubble n) house.position 0 (layers.bubble, 0)]

    renderCustomer =
      case IHopeItWorks.find (Customer.livesHere house) customers of
        Nothing ->
          []
        Just customer ->
          if customer.happiness == 2 && not hasRequests && not hasArticles then
            []
          else
            CustomerView.render requestsFromHouse deliveredArticles house customer
  in
    [ Box.offsetTextured (0, -1) Textures.House position 0 (layers.obstacle, 0)
    , Box.offsetTextured (0, 1) Textures.HouseShadow position 0 (layers.shadow, 0)
    , Box.clickable (2, 3) (0, -1) position (layers.click, 0) (Actions.ClickMapObject house)
    ]
    ++ List.concat (List.indexedMap renderRequest requestsFromHouse)
    ++ renderBubble
    ++ renderCustomer
