port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import TestSvg exposing (..)
--import Playground exposing (..)

port sendStuff : Json.Encode.Value -> Cmd msg


port receiveStuff : (Json.Encode.Value -> msg) -> Sub msg

---- MODEL ----

type Model
    = Loading
    | Running GameState
    | Error


type alias GameState =
    { phrase : String
    , guesses : Set String
    , counter : Int 
    }


init : ( Model, Cmd Msg )
init =
    ( Loading
    , fetchWord
    )


---- UPDATE ----


type Msg
    = Guess String
    | Restart
    | NewPhrase (Result Http.Error String)
    | SendData
    | Received (Result Decode.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess char ->
            case model of
                Running gameState ->
                    (  Running { gameState | guesses = Set.insert char gameState.guesses }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Restart ->
            ( Loading, fetchWord )

        NewPhrase result ->
            case result of
                Ok phrase ->
                    ( Running { phrase = phrase, guesses = Set.empty, counter = 0 }, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )

        SendData ->
            ( model, sendStuff <| Json.Encode.string "Test" )

        Received result ->
            case result of
                Ok value ->
                    case model of
                        Running gameState ->
                            ( Running { gameState | counter = value }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err error ->
                    ( Error, Cmd.none )
                    --( { model | error = Decode.errorToString error }, Cmd.none )


fetchWord : Cmd Msg
fetchWord =
    Http.get
        { url = "http://snapdragon-fox.glitch.me/word"
        , expect = Http.expectJson NewPhrase wordDecoder
        }


wordDecoder : Decoder String
wordDecoder =
    Decode.field "word" Decode.string

valueDecoder : Decoder Int
valueDecoder =
    Decode.field "value" Decode.int

subscriptions : Model -> Sub Msg
subscriptions model =
    receiveStuff (Decode.decodeValue valueDecoder >> Received)

---- VIEW ----


view : Model -> Html Msg
view model =
    case model of 
        Loading ->
            div [] [ Html.text "Loading" ]

        Running gameState ->
            viewGameState gameState

        Error ->
            div [] [ Html.text "Error" ]
{-
drawHangman =
  picture
    [ rectangle brown 40 200
    , circle green 100
        |> moveUp 100
    ]
-}
showSVG : Html msg
showSVG =
    svg
        [ viewBox "0 0 400 400"
        , Svg.Attributes.width "400"
        , Svg.Attributes.height "400"
        ]
        [ circle
            [ cx "50"
            , cy "50"
            , r "40"
            , fill "red"
            , stroke "black"
            , strokeWidth "3"
            ]
            []
        , rect
            [ x "100"
            , y "10"
            , Svg.Attributes.width "40"
            , Svg.Attributes.height "40"
            , fill "green"
            , stroke "black"
            , strokeWidth "2"
            ]
            []
        , line
            [ x1 "20"
            , y1 "200"
            , x2 "200"
            , y2 "20"
            , stroke "blue"
            , strokeWidth "10"
            , strokeLinecap "round"
            ]
            []
        , polyline
            [ points "200,40 240,40 240,80 280,80 280,120 320,120 320,160"
            , fill "none"
            , stroke "red"
            , strokeWidth "4"
            , strokeDasharray "20,2"
            ]
            []
        , text_
            [ x "130"
            , y "130"
            , fill "black"
            , textAnchor "middle"
            , dominantBaseline "central"
            , transform "rotate(-45 130,130)"
            ]
            [ Svg.text "Welcome to Shapes Club"
            ]
        ]

showHangman : Int -> Html msg
showHangman count =
    case count of
        0 ->
            div [] []
        1 ->
            img [src "test.png"] []
        2 ->
            showSVG
        3 ->
            showTestSVG
        4 ->
            div
                [ Html.Attributes.style "width" "200px"
                , Html.Attributes.style "height" "200px"
                , Html.Attributes.style "background-image" "url('test.png')"
                , Html.Attributes.style "background-position" "center"
                , Html.Attributes.style "background-repeat" "no-repeat"
                , Html.Attributes.style "background-size" "contain"
                ]
                []
        _ ->
            div
                [ Html.Attributes.style "width" "200px"
                , Html.Attributes.style "height" "300px"
                , Html.Attributes.style "border" "2px solid blue"
                ]
                []

viewInt : Int -> Html Msg
viewInt count =
    div []
        [ div [] [ Html.text (String.fromInt count) ]
        ]

viewCounter : Int -> Html Msg
viewCounter counter =
    div []
        [ div [] [ Html.text (String.fromInt counter) ]
        ]

viewGameState : GameState -> Html Msg
viewGameState gameState =
    let
        phraseHtml =
            gameState.phrase
                |> String.split ""
                |> List.map
                    (\char ->
                        if char == " " then
                            " "

                        else if Set.member char gameState.guesses then
                            char

                        else
                            "_"
                    )
                |> List.map
                    (\char ->
                        span [] [ Html.text char ]
                    )
                |> div []

        phraseSet =
            gameState.phrase
            |> String.split ""
            |> Set.fromList

        failuresHtml =
            gameState.guesses
            |> Set.toList
            |> List.filter (\char -> not <| Set.member char phraseSet)
            |> List.map (\char -> span [] [ Html.text char ])
            |> div []

        hangmanHtml =
            gameState.guesses
            |> Set.toList
            |> List.filter (\char -> not <| Set.member char phraseSet)
            |> List.length
            |> viewInt

        counterHtml =
            gameState.counter
            |> viewCounter

        hangmanImageHtml =
            gameState.guesses
            |> Set.toList
            |> List.filter (\char -> not <| Set.member char phraseSet)
            |> List.length
            |> showHangman

        buttonsHtml =
            "abcdefghijklmnopqrstuv"
            |> String.split ""
            |> List.map (\char ->
                button [ onClick <| Guess char ] [ Html.text char ]
                )
            |> div []

    in
    div []
        [ phraseHtml
        , buttonsHtml
        , failuresHtml
        , button [ onClick Restart ] [ Html.text "Restart" ]
        , hangmanHtml
        , hangmanImageHtml
        , button [ onClick SendData ] [ Html.text "Send Data" ]
        , h2 [] [ Html.text "Hmm" ]
        , counterHtml
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
