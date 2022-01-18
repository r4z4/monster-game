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
import Counter exposing (..)
--import Playground exposing (..)

port sendStuff : Json.Encode.Value -> Cmd msg


port receiveStuff : (Json.Encode.Value -> msg) -> Sub msg

---- MODEL ----

type Model
    = Loading
    | Running GameState
    | GameOver GameState
    | Error

type alias Flags =
    { src : String
    }

type alias GameState =
    { phrase : String
    , guesses : Set String
    , wrongGuesses : List String
    , stillUnderscores : Bool
    , counter : Int 
    , gameOver : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Loading
    , fetchWord
    )


---- UPDATE ----


type Msg
    = Guess String
    | Restart
    | NewPhrase (Result Http.Error String)
    --| EndOfGame Bool --Bool being True if won, False if lost
    | SendData Int
    | Received (Result Decode.Error Int) 
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model, Cmd.none )

        Guess char ->
            case model of
                Running gameState ->
                    (  Running { gameState | guesses = Set.insert char gameState.guesses 
                               , wrongGuesses = List.filter (\char_ -> not <| String.contains char_ gameState.phrase) <| Set.toList <| Set.insert char gameState.guesses 
                               , stillUnderscores = seeIfBlanks <| List.map
                                                                                    (\char2 ->
                                                                                        if char2 == " " then
                                                                                            " "

                                                                                        else if Set.member char2 gameState.guesses then
                                                                                            char2

                                                                                        else
                                                                                            "_"
                                                                                    ) <| String.split "" <| gameState.phrase
                               }, sendStuff <| Json.Encode.int <| Set.size gameState.guesses )

                _ ->
                    ( model, Cmd.none )

        Restart ->
            ( Loading, fetchWord )

        NewPhrase result ->
            case result of
                Ok phrase ->
                    ( Running { phrase = phrase
                              , guesses = Set.empty
                              , counter = 0
                              , gameOver = False
                              , wrongGuesses = [] 
                              , stillUnderscores = True
                              }, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )
{--
        EndOfGame bool ->
            case bool of
                True ->
                    ( GameOver True, Cmd.none )
                False ->
                    ( model, Cmd.none )
--}
        SendData int ->
            ( model, sendStuff <| Json.Encode.int int )

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
        { url = "https://random-word-form.herokuapp.com/random/animal"
        , expect = Http.expectJson NewPhrase indexDecoder
        }
-- Old url = "http://snapdragon-fox.glitch.me/word"
{--
lostWhenSeven : Int -> Msg
lostWhenSeven int =
        case int of
            7 ->
                ( EndOfGame False)
            _ ->
                ( EndOfGame True)
--}
wordDecoder : Decoder String
wordDecoder =
    Decode.field "word" Decode.string

indexDecoder : Decoder String
indexDecoder =
    Decode.index 0 Decode.string

valueDecoder : Decoder Int
valueDecoder =
    Decode.field "value" Decode.int

boolDecoder : Decoder Bool
boolDecoder =
    Decode.field "gameOver" Decode.bool

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
            case List.length gameState.wrongGuesses of
                7 ->
                    viewGameOver gameState
                _ ->
                    case gameState.stillUnderscores of
                        True ->
                            viewGameState gameState
                        False ->
                            viewGameOver gameState


        GameOver gameState ->
            viewGameOver gameState

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

seeIfBlanks : List String -> Bool
seeIfBlanks list =
    List.member "_" list
{--
winWhenZero : Bool -> Int
winWhenZero bool =
        case bool of
            False ->
                div [ Html.Attributes.class "gameover" ] [ Html.text "Congrats! You have stumped the monster! Would you like to try again and continue defending the world?"]
            True ->
                div [ Html.Attributes.class "gameover" ] [] 
--}
winWhenZeroHtml : Bool -> Html Msg
winWhenZeroHtml bool =
        case bool of
            False ->
                div [ Html.Attributes.class "gameover" ] [ Html.text "Congrats! You have stumped the monster! Guess any letter once more to see him destroyed."]
            True ->
                div [ Html.Attributes.class "gameover" ] [] 


showHangman : Int -> Html Msg
showHangman count =
    case count of
        0 ->
            div [ Html.Attributes.id "monster" ] []
        1 ->
            div [ Html.Attributes.id "monster" ] [ img [src "./Capture6.png"] [] ]
        2 ->
            div [ Html.Attributes.id "monster" ] [ img [src "./Capture5.png"] [] ]
        3 ->
            div [ Html.Attributes.id "monster" ] [ img [src "./Capture4.png"] [] ]
        4 ->
            div [ Html.Attributes.id "monster" ] [ img [src "./Capture3.png"] [] ]
        5 ->
            div [ Html.Attributes.id "monster" ] [ img [src "./Capture2.png"] [] ]
        6 ->
            div [ Html.Attributes.id "monster" ] [ img [src "./Capture1.png"] [] ]
        7 ->
            div [ Html.Attributes.id "endmonster" ] [ div [] [ h2 [] [ Html.text "Game Over" ]
                            , img [src "./Capture.png"] []
                            , button [ onClick Restart ] [ Html.text "Play Again" ]
                    ]
            ]
        8 ->
            div [] [ img [src "https://static.wikia.nocookie.net/versus-compendium/images/3/39/Link_ALTTP.png"] [] ]
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

viewGameOver : GameState -> Html Msg
viewGameOver gameState =
    case gameState.stillUnderscores of
        True ->
            div [] [ 
                div [] [ h1 [] [ Html.text "Game Over. You have failed your fellow man. The Monster will keep growing." ]
                       , br [] []
                       , img [src "./Capture.png", Html.Attributes.class "losingEnd" ] [] 
                       , br [] []
                       , button [ onClick Restart ] [ Html.text "Play Again" ] ]
                       ]
        False ->
            div [] [ 
                div [] [ h1 [] [ Html.text "You did it! You have saved the world from this awful Monster! Be gone with him!" ]
                       , img [ src "./Capture.png", Html.Attributes.class "winningEnd" ] []
                       , br [] [] 
                       , button [ onClick Restart ] [ Html.text "Play Again" ] ]
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
                |> div [ Html.Attributes.class "underscores" ]

        phraseSet =
            gameState.phrase
            |> String.split ""
            |> Set.fromList

        failuresHtml =
            gameState.guesses
            |> Set.toList
            |> List.filter (\char -> not <| Set.member char phraseSet)
            |> List.map (\char -> span [] [ Html.text char ])
            |> div [ Html.Attributes.class "failures" ]

        hangmanHtml =
            gameState.guesses
            |> Set.toList
            |> List.filter (\char -> not <| Set.member char phraseSet)
            |> List.length
            |> viewInt

        counterHtml =
            gameState.counter
            |> viewCounter

        gameOverHtml =
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
            |> seeIfBlanks
            |> winWhenZeroHtml

        hangmanImageHtml =
            gameState.guesses
            |> Set.toList
            |> List.filter (\char -> not <| Set.member char phraseSet)
            |> List.length
            |> showHangman

        buttonsHtml =
            "abcdefghijklmnopqrstuvwxyz"
            |> String.split ""
            |> List.map (\char ->
                button [ onClick <| Guess char ] [ Html.text char ]
                )
            |> div [ Html.Attributes.class "letterBtns" ]

    in
    div []
        [ h1 [] [ Html.text "Stump the Monster Letter Game" ]
        , h4 [] [ Html.text "Don't Let Him Grow" ]
        , phraseHtml
        , buttonsHtml
        , div [ Html.Attributes.class "incorrect"] [
            h2 [] [ Html.text "Incorrect Guesses" ]
            , failuresHtml
        ]
        , hangmanHtml
        , hangmanImageHtml
        , gameOverHtml
        , counterHtml
        , div [ Html.Attributes.id "resignArea" ] [ h3 [] [ Html.text "Feel Free to Start Over if You Can't Handle it"]
                                                  , button [ onClick Restart, Html.Attributes.class "btnClass" ] [ Html.text "Resign" ]
                                                  ]
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
