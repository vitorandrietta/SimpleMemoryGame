module Main exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import Task
import Time


--Model


cardsPath : List String
cardsPath =
    [ "img/BolvarFrodragon.jpg"
    , "img/DeathWing.jpg"
    , "img/Nozdormu.jpg"
    , "img/Ragnaros.png"
    , "img/Sindragosa.jpg"
    , "img/TheLichKing.jpg"
    , "img/TirionFordring.jpg"
    , "img/YoogSaron.jpg"
    ]


cardBack : String
cardBack =
    "img/CardBack.png"


type alias Card =
    { img : String
    , upside : Bool
    , id : Int
    }


type alias Model =
    { board : List Card
    , firstFlip : Maybe Card
    , secondFlip : Maybe Card
    }


type Msg
    = RestartGame
    | PickFirst Card --ok
    | PickSecond Card
    | PickAlreadyFlipped --ok
    | EndAPlay --ok
    | ShuffleBoard (List Card) --ok


initModel : ( Model, Cmd Msg )
initModel =
    let
        cards =
            (cardsPath ++ cardsPath)
                |> List.foldl
                    (\card cards ->
                        Card card False (List.length cards) :: cards
                    )
                    []

        model =
            Model cards Nothing Nothing
    in
    ( model, generate ShuffleBoard (shuffle model.board) )



--View


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ button [ type_ "button", class "btn btn-primary restart", onClick RestartGame ]
            [ text "Reiniciar Jogo" ]
        , div [ class "jumbotron" ]
            (if gameOver model.board then
                [ text "fim de Jogo" ]
             else
                []
            )
        , div []
            [ board model ]
        ]


delay : Time.Time -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


boardToArray : List Card -> Array.Array (Array.Array Card)
boardToArray cards =
    let
        cardsArray =
            Array.fromList cards

        line1 =
            Array.slice 0 4 cardsArray

        line2 =
            Array.slice 4 8 cardsArray

        line3 =
            Array.slice 8 12 cardsArray

        line4 =
            Array.slice 12 16 cardsArray
    in
    Array.fromList [ line1, line2, line3, line4 ]


board : Model -> Html Msg
board model =
    Array.map (boardLine model)
        (boardToArray model.board)
        |> Array.toList
        |> div [ class "board text-center" ]



--Update


boardLine : Model -> Array.Array Card -> Html Msg
boardLine model line =
    Array.map (boardCell model) line
        |> Array.toList
        |> div [ class "row" ]


boardCell : Model -> Card -> Html Msg
boardCell model card =
    div
        [ class "col col-lg-3 cr"
        , onClick
            (if not card.upside then
                if model.firstFlip == Nothing then
                    PickFirst card
                else
                    PickSecond card
             else
                PickAlreadyFlipped
            )
        ]
        [ img
            [ src
                (if card.upside then
                    card.img
                 else
                    cardBack
                )
            ]
            []
        ]


flipCard : Int -> Model -> Model
flipCard id model =
    let
        updatedCards =
            List.map
                (\card ->
                    if card.id == id then
                        Card card.img (not card.upside) id
                    else
                        card
                )
                model.board
    in
    { model | board = updatedCards }


extractCard : Maybe Card -> Card
extractCard card =
    case card of
        Just val ->
            val

        _ ->
            Card "" False 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleBoard shuffledCards ->
            { model | board = shuffledCards } ! []

        PickFirst card ->
            let
                updatedModel =
                    flipCard card.id model
            in
            { updatedModel | firstFlip = Just card } ! []

        PickSecond card ->
            let
                updatedModel =
                    flipCard card.id model
            in
            ( { updatedModel | secondFlip = Just card }, delay 400 EndAPlay )

        PickAlreadyFlipped ->
            model ! []

        EndAPlay ->
            let
                firstFlipped =
                    extractCard model.firstFlip

                secondFlipped =
                    extractCard model.secondFlip

                newModel =
                    if
                        firstFlipped.img
                            == secondFlipped.img
                    then
                        model
                    else
                        flipCard firstFlipped.id model
                            |> flipCard secondFlipped.id
            in
            { newModel | firstFlip = Nothing, secondFlip = Nothing } ! []

        RestartGame ->
            initModel


gameOver : List Card -> Bool
gameOver cards =
    List.all flipped cards


flipped : Card -> Bool
flipped card =
    card.upside


main : Program Never Model Msg
main =
    Html.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
