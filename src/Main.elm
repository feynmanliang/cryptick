module Main exposing (..)

import Debug
import Date exposing (Date)
import Date.Extra.Format as Format
import Dict
import Html exposing (..)
import Html.Attributes exposing (src, class, id, type_, attribute)
import Json.Decode as Decode exposing (andThen)
import WebSocket


---- MODEL ----


gdaxWS : String
gdaxWS =
    "wss://ws-feed.gdax.com"


type alias Price =
    Float



-- NOTE: id is a String because UDTs are not comparable
-- see https://github.com/elm-lang/elm-compiler/issues/774


type alias Product =
    { id : String, price : Price, datetime : Date }


type alias Model =
    { products : Dict.Dict String Product
    , message : String
    , lastUpdated : Maybe Date
    }


init : ( Model, Cmd Msg )
init =
    ( { products = Dict.empty
      , message = ""
      , lastUpdated = Nothing
      }
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
    )


decodeStringToPrice : String -> Decode.Decoder Price
decodeStringToPrice x =
    case String.toFloat x of
        Ok n ->
            Decode.succeed n

        Err _ ->
            Decode.fail "could not parse floating point price"


decodeStringToDatetime : String -> Decode.Decoder Date
decodeStringToDatetime x =
    case Date.fromString x of
        Ok datetime ->
            Decode.succeed datetime

        _ ->
            Decode.fail "could not parse datetime"


decodePriceResponse : Decode.Decoder Price
decodePriceResponse =
    (Decode.field "last" Decode.string)
        |> andThen decodeStringToPrice


decodeTickerResponse : Decode.Decoder Product
decodeTickerResponse =
    Decode.map3 Product
        (Decode.field "product_id" Decode.string)
        ((Decode.field "price" Decode.string) |> andThen decodeStringToPrice)
        ((Decode.field "time" Decode.string) |> andThen decodeStringToDatetime)



---- UPDATE ----


type Msg
    = NoOp
    | ReceiveLastUpdated Date
    | NewPriceWS String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewPriceWS message ->
            case (Decode.decodeString decodeTickerResponse message) of
                Ok newProduct ->
                    ( { model
                        | products = Dict.insert newProduct.id newProduct model.products
                        , lastUpdated = Just newProduct.datetime
                      }
                    , Cmd.none
                    )

                Err err ->
                    let
                        _ =
                            Debug.log "err" err
                    in
                        ( model, Cmd.none )

        ReceiveLastUpdated date ->
            ( { model | lastUpdated = Just date }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen gdaxWS NewPriceWS



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        cellFor productId =
            div [ class "col-sm-4 text-center" ]
                [ text
                    (productId
                        ++ ": "
                        ++ (Maybe.withDefault
                                "Loading"
                                (Dict.get productId model.products
                                    |> (Maybe.map (\x -> toString x.price))
                                )
                           )
                    )
                ]
    in
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
                ]
            , div [ class "container" ]
                [ div [ class "row" ]
                    [ cellFor "BTC-USD"
                    , cellFor "LTC-USD"
                    , cellFor "ETH-USD"
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
