module Main exposing (..)

import Date exposing (Date)
import Date.Extra.Format as Format
import Html exposing (..)
import Html.Attributes exposing (src, class, id, type_, attribute)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (andThen)
import Task


---- MODEL ----


type alias Price =
    Float


type alias Model =
    { bitcoinPrice : Maybe Price
    , lastUpdated : Maybe Date
    }


init : ( Model, Cmd Msg )
init =
    ( { bitcoinPrice = Nothing
      , lastUpdated = Nothing
      }
    , getPrice BTC GDAX
    )


getPrice : Currency -> Exchange -> Cmd Msg
getPrice currency exchange =
    case exchange of
        GDAX ->
            case currency of
                BTC ->
                    Http.send NewPrice
                        (Http.get "https://api.gdax.com/products/BTC-USD/stats"
                            (decodeResponse currency exchange)
                        )


decodeResponse : Currency -> Exchange -> Decode.Decoder ( Currency, Exchange, Price )
decodeResponse currency exchange =
    let
        stringToPrice x =
            case String.toFloat x of
                Ok n ->
                    Decode.succeed ( currency, exchange, n )

                Err _ ->
                    Decode.fail "could not parse string into a floating point price"
    in
        (Decode.field "last" Decode.string)
            |> andThen stringToPrice



---- UPDATE ----


type Exchange
    = GDAX


type Currency
    = BTC


type Msg
    = NoOp
    | NewPrice (Result Http.Error ( Currency, Exchange, Float ))
    | RefreshPrice ( Currency, Exchange )
    | ReceiveLastUpdated Date


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewPrice (Ok ( currency, exchange, price )) ->
            ( { model | bitcoinPrice = Just price }, Task.perform ReceiveLastUpdated Date.now )

        NewPrice (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
                ( model, Cmd.none )

        RefreshPrice ( currency, exchange ) ->
            ( model, getPrice currency exchange )

        ReceiveLastUpdated date ->
            ( { model | lastUpdated = Just date }, Cmd.none )



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
            , a [ class "btn btn-primary text-white", onClick (RefreshPrice ( BTC, GDAX )) ] [ text "Refresh prices" ]
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
        , subscriptions = always Sub.none
        }
