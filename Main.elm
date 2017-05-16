module Main exposing (..)

import Html exposing (Html, div, text, button, ul, li, input)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Table


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
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
    , dailyTableState : Table.State
    , completedTableState : Table.State
    , queueTableState : Table.State
    , backlogTableState : Table.State
    }


type Msg
    = Add
    | Move Movement Int
    | NoteContent String
    | SetTableState Stage Table.State


tableInit =
    Table.initialSort "Title"


initModel : Model
initModel =
    { notes = []
    , id = 0
    , textField = ""
    , dailyTableState = tableInit
    , completedTableState = tableInit
    , queueTableState = tableInit
    , backlogTableState = tableInit
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


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
update msg model =
    case msg of
        Add ->
            let
                note =
                    Note (model.id + 1) model.textField Daily

                newNotes =
                    note :: model.notes

                newId =
                    model.id + 1
            in
                ( { model | notes = newNotes, id = newId }, Cmd.none )

        Move movement noteId ->
            case movement of
                Forward ->
                    let
                        movedNotes =
                            List.map (forward noteId) model.notes
                    in
                        ( { model | notes = movedNotes }, Cmd.none )

                Backward ->
                    let
                        movedNotes =
                            List.map (backward noteId) model.notes
                    in
                        ( { model | notes = movedNotes }, Cmd.none )

        NoteContent str ->
            ( { model | textField = str }, Cmd.none )

        SetTableState stage state ->
            case stage of
                Daily ->
                    ( { model | dailyTableState = state }, Cmd.none )

                Completed ->
                    ( { model | completedTableState = state }, Cmd.none )

                Queue ->
                    ( { model | queueTableState = state }, Cmd.none )

                Backlog ->
                    ( { model | backlogTableState = state }, Cmd.none )


stageIsEqual : Stage -> Note -> Bool
stageIsEqual stage note =
    note.stage == stage


filterStage : Stage -> List Note -> List Note
filterStage stage notes =
    List.filter (stageIsEqual stage) notes


stageView : String -> Stage -> List Note -> Table.Config Note Msg -> Table.State -> Html Msg
stageView header stage notes tableConfig tableState =
    div [ class "stageViewDiv" ]
        [ text header
        , Table.view tableConfig tableState (filterStage stage notes)
        ]


titleColumn : Table.Column Note Msg
titleColumn =
  Table.veryCustomColumn
    { name = "Title"
    , viewData = noteView
    , sorter = Table.increasingOrDecreasingBy .content
    }

noteView : Note -> Table.HtmlDetails Msg
noteView note =
    Table.HtmlDetails []
        [ text note.content
        , button [ onClick (Move Forward note.id) ] [ text "✓" ]
        , button [ onClick (Move Backward note.id) ] [ text "✗" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "newNoteDiv" ]
            [ input [ onInput NoteContent ] []
            , button [ onClick Add ] [ text "Add to daily" ]
            ]
        , div [ class "stageViewContainer" ]
            [ stageView "Backlog" Backlog model.notes (configGen Backlog) model.backlogTableState
            , stageView "Queue" Queue model.notes (configGen Queue) model.queueTableState
            , stageView "Daily" Daily model.notes (configGen Daily) model.dailyTableState
            , stageView "Completed" Completed model.notes (configGen Completed) model.completedTableState
            , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
            ]
        ]


toId : Note -> String
toId note =
    toString note.id


configGen : Stage -> Table.Config Note Msg
configGen stage =
    Table.config
        { toId = toId
        , toMsg = (SetTableState stage)
        , columns =
            [ titleColumn
            ]
        }
