module Main exposing (..)

import Html exposing (Html, div, text, button, ul, li, input)
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
    , textField : String
    }


type Msg
    = Add
    | Move Movement Int
    | NoteContent String


init : ( Model, Cmd Msg )
init =
    ( Model [] 0 "", Cmd.none )


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
update msg { notes, id, textField } =
    case msg of
        Add ->
            let
                note =
                    Note (id + 1) textField Daily
            in
                ( Model (note :: notes) (id + 1) textField, Cmd.none )

        Move movement noteId ->
            case movement of
                Forward ->
                    ( Model (List.map (forward noteId) notes) id textField, Cmd.none )

                Backward ->
                    ( Model (List.map (backward noteId) notes) id textField, Cmd.none )

        NoteContent str ->
            ( Model notes id str, Cmd.none )


stageIsEqual : Stage -> Note -> Bool
stageIsEqual stage note =
    note.stage == stage


filterStage : Stage -> List Note -> List Note
filterStage stage notes =
    List.filter (stageIsEqual stage) notes


noteView : Note -> Html Msg
noteView note =
    li []
        [ text note.content
        , button [ onClick (Move Forward note.id) ] [ text "✓" ]
        , button [ onClick (Move Backward note.id) ] [ text "✗" ]
        ]


stageView : String -> Stage -> List Note -> Html Msg
stageView header stage notes =
    div [ class "stageViewDiv" ]
        [ text header
        , ul [] (filterStage stage notes |> List.map noteView)
        ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "newNoteDiv" ]
            [ input [ onInput NoteContent ] []
            , button [ onClick Add ] [ text "Add to daily" ]
            ]
        , div [ class "stageViewContainer" ]
            [ stageView "Backlog" Backlog model.notes
            , stageView "Queue" Queue model.notes
            , stageView "Daily" Daily model.notes
            , stageView "Completed" Completed model.notes
            , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
            ]
        ]
