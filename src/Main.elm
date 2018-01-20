module Main exposing (..)

import Debug
import Html exposing (Html, text, div, h1, img, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (andThen)


---- MODEL ----


type alias Price =
    Float


type alias Model =
    { bitcoinPrice : Maybe Price }


init : ( Model, Cmd Msg )
init =
    ( { bitcoinPrice = Nothing
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewPrice (Ok ( currency, exchange, price )) ->
            ( { model | bitcoinPrice = Just price }, Cmd.none )

        NewPrice (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
                ( model, Cmd.none )

        RefreshPrice ( currency, exchange ) ->
            ( model, getPrice currency exchange )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        bitcoinElem =
            case model.bitcoinPrice of
                Nothing ->
                    "Loading"

                Just p ->
                    toString p
    in
        div []
            [ h1 [] [ text "GDAX Bitcoin Price" ]
            , div [] [ text bitcoinElem ]
            , button [ onClick (RefreshPrice ( BTC, GDAX )) ] [ text "Refresh" ]
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
