module Main exposing (..)

import Debug
import Date exposing (Date)
import Date.Extra.Format as Format
import Html exposing (..)
import Html.Attributes exposing (src, class, id, type_, attribute)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (andThen)
import Task
import WebSocket


---- MODEL ----


gdaxWS : String
gdaxWS =
    "wss://ws-feed.gdax.com"


type ProductID
    = BTC_USD
    | ETH_USD
    | LTC_USD


type alias Price =
    Float


type alias Product =
    { id : ProductID, price : Price }


type alias Model =
    { bitcoinPrice : Maybe Price
    , message : String
    , lastUpdated : Maybe Date
    }


init : ( Model, Cmd Msg )
init =
    ( { bitcoinPrice = Nothing
      , message = ""
      , lastUpdated = Nothing
      }
    , Cmd.batch
        [ getPrice
        , WebSocket.send gdaxWS """
{
    "type": "subscribe",
    "product_ids": [
        "BTC-USD",
        "ETH-USD",
        "LTC-USD"
    ],
    "channels": [
        "ticker"
    ]
}
"""
        ]
    )


getPrice : Cmd Msg
getPrice =
    Http.send NewPrice
        (Http.get "https://api.gdax.com/products/BTC-USD/stats"
            (decodePriceResponse)
        )


decodeStringToPrice : String -> Decode.Decoder Price
decodeStringToPrice x =
    case String.toFloat x of
        Ok n ->
            Decode.succeed n

        Err _ ->
            Decode.fail "could not parse floating point price"


decodeProductID : String -> Decode.Decoder ProductID
decodeProductID x =
    case x of
        "BTC-USD" ->
            Decode.succeed BTC_USD

        "ETH-USD" ->
            Decode.succeed ETH_USD

        "LTC-USD" ->
            Decode.succeed LTC_USD

        _ ->
            Decode.fail "could not parse product"


decodePriceResponse : Decode.Decoder Price
decodePriceResponse =
    (Decode.field "last" Decode.string)
        |> andThen decodeStringToPrice


decodeTickerResponse : Decode.Decoder Product
decodeTickerResponse =
    Decode.map2 Product
        ((Decode.field "product_id" Decode.string) |> andThen decodeProductID)
        ((Decode.field "price" Decode.string) |> andThen decodeStringToPrice)



---- UPDATE ----


type Msg
    = NoOp
    | NewPrice (Result Http.Error Float)
    | RefreshPrice
    | ReceiveLastUpdated Date
    | NewPriceWS String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewPrice (Ok price) ->
            ( { model | bitcoinPrice = Just price }, Task.perform ReceiveLastUpdated Date.now )

        NewPrice (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
                ( model, Cmd.none )

        NewPriceWS message ->
            let
                _ =
                    Debug.log "msg" (Decode.decodeString decodeTickerResponse message)
            in
                ( { model | message = message }, Cmd.none )

        RefreshPrice ->
            ( model, getPrice )

        ReceiveLastUpdated date ->
            ( { model | lastUpdated = Just date }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen gdaxWS NewPriceWS



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ header []
            [ nav [ class "navbar", class "navbar-dark", class "bg-dark" ]
                [ div [ class "container", class "d-flex", class "justify-content-between" ]
                    [ span [ class "navbar-brand", class "d-flex", class "align-items-center" ]
                        [ span [ id "logo" ] [ text "CT" ]
                        , strong [] [ text "CrypTick" ]
                        ]
                    ]
                ]
            ]
        , div [ class "jumbotron text-center" ]
            [ h1 [ class "display-4" ] [ text "Cryptocurrency price ticker" ]
            , p [ class "lead" ]
                [ text ("Last updated " ++ (Maybe.withDefault "never" (Maybe.map Format.isoString model.lastUpdated)))
                ]
            , a [ class "btn btn-primary text-white", onClick (RefreshPrice) ] [ text "Refresh prices" ]
            ]
        , div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col-sm-4 text-center" ]
                    [ text ("Bitcoin (GDAX): " ++ (Maybe.withDefault "Loading" (Maybe.map toString model.bitcoinPrice)))
                    ]
                , div [ class "col-sm-4 text-center" ] [ text "One of three columns" ]
                , div [ class "col-sm-4 text-center" ] [ text "One of three columns" ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
