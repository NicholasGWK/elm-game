module Main exposing (..)

import Html exposing (Html, div, text, button, ul, li, input)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Table
import DnD


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


dnd =
    DnD.init DnDMsg Dropped


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model.draggable
        ]


type Stage
    = Backlog
    | Queue
    | Daily
    | Completed


type alias NoteId =
    Int


type alias Model =
    { notes : List Note
    , id : NoteId
    , textField : String
    , dailyTableState : Table.State
    , completedTableState : Table.State
    , queueTableState : Table.State
    , backlogTableState : Table.State
    , draggable : DnD.Draggable Stage Note
    }


type alias Note =
    { id : NoteId
    , content : String
    , stage : Stage
    }


type Msg
    = Add
    | Move Stage NoteId
    | NoteContent String
    | SetTableState Stage Table.State
    | Dropped Stage Note
    | DnDMsg (DnD.Msg Stage Note)


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
    , draggable = dnd.model
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


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

        Move stage noteId ->
            let
                movedNotes =
                    List.map (updateStageIfIdMatch noteId stage) model.notes
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

        DnDMsg msg ->
            ( { model | draggable = DnD.update msg model.draggable }, Cmd.none )

        Dropped stage note ->
            let
                movedNotes =
                    List.map (updateStageIfIdMatch note.id stage) model.notes
            in
                ( { model | notes = movedNotes }, Cmd.none )


forward : Note -> Msg
forward note =
    let
        nextStage =
            case note.stage of
                Backlog ->
                    Queue

                Queue ->
                    Daily

                Daily ->
                    Completed

                Completed ->
                    Completed
    in
        Move nextStage note.id


backward : Note -> Msg
backward note =
    let
        nextStage =
            case note.stage of
                Backlog ->
                    Backlog

                Queue ->
                    Backlog

                Daily ->
                    Queue

                Completed ->
                    Daily
    in
        Move nextStage note.id


updateStageIfIdMatch : NoteId -> Stage -> Note -> Note
updateStageIfIdMatch id stage note =
    if id == note.id then
        { note | stage = stage }
    else
        note


stageIsEqual : Stage -> Note -> Bool
stageIsEqual stage note =
    note.stage == stage


filterStage : Stage -> List Note -> List Note
filterStage stage notes =
    List.filter (stageIsEqual stage) notes


stageView : String -> Stage -> List Note -> Table.Config Note Msg -> Table.State -> Html Msg
stageView header stage notes tableConfig tableState =
    dnd.droppable stage
        [class "stageViewDiv" ]
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
        [ (dnd.draggable note
            []
            [ text note.content
            , button [ onClick (forward note) ] [ text "✓" ]
            , button [ onClick (backward note) ] [ text "✗" ]
            ]
          )
        ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "newNoteDiv" ]
            [ input [ onInput NoteContent ] []
            , button [ onClick Add ] [ text "Add to Daily" ]
            ]
        , div [ class "stageViewContainer" ]
            [ stageView "Backlog" Backlog model.notes (configGen Backlog) model.backlogTableState
            , stageView "Queue" Queue model.notes (configGen Queue) model.queueTableState
            , stageView "Daily" Daily model.notes (configGen Daily) model.dailyTableState
            , stageView "Completed" Completed model.notes (configGen Completed) model.completedTableState
            , DnD.dragged model.draggable dragged
            , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
            ]
        ]


dragged : Note -> Html Msg
dragged note =
    div [] [ text note.content ]


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
