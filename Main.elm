module Main exposing (..)

import Html exposing (Html, div, text, button, ul, li)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Stage
    = Backlog
    | Queue
    | Daily
    | Completed


type Movement
    = Forward
    | Backward


type alias Note =
    { id : Int
    , content : String
    , stage : Stage
    }


type alias Model =
    { notes : List Note
    , id : Int
    }


type Msg
    = Add Note
    | Move Movement Int


init : ( Model, Cmd Msg )
init =
    ( Model [] 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


forward : Int -> Note -> Note
forward noteId { id, stage, content } =
    if id == noteId then
        case stage of
            Backlog ->
                Note id content Queue

            Queue ->
                Note id content Daily

            Daily ->
                Note id content Completed

            Completed ->
                Note id content Completed
    else
        Note id content stage


backward : Int -> Note -> Note
backward noteId { id, content, stage } =
    if id == noteId then
        case stage of
            Backlog ->
                Note id content Backlog

            Queue ->
                Note id content Backlog

            Daily ->
                Note id content Queue

            Completed ->
                Note id content Daily
    else
        Note id content stage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { notes, id } =
    case msg of
        Add note ->
            ( Model (note :: notes) (id + 1), Cmd.none )

        Move movement noteId ->
            case movement of
                Forward ->
                    ( Model (List.map (forward noteId) notes) id, Cmd.none )

                Backward ->
                    ( Model (List.map (backward noteId) notes) id, Cmd.none )


stageView : String -> Stage -> List Note -> Html Msg
stageView header stage notes =
    div []
        [ text header
        , ul [] (List.filter (\note -> note.stage == stage) notes |> (List.map (\note -> li [] [ text note.context ])))
        ]


view : Model -> Html Msg
view model =
    div [] [ stageView "Daily" Daily model.notes ]
