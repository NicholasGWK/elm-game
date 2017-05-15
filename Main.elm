module Main exposing (..)

import Html exposing (Html, div, text, button)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { startTime : Int
    , interval : Interval
    , running : Bool
    , currentTime: Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model (25 * 60 * 60) Pomodoro False, Task.perform GetTime Time.now )


pomodoroTime =
    25 * 60


shortTime =
    5 * 60


longTime =
    10 * 60


type Interval
    = Pomodoro
    | Short
    | Long


type Msg
    = SetInterval Interval
    | Start
    | Stop
    | Tick Time
    | SetStartTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { startTime, interval, running, currentTime } =
    case msg of
        SetStartTime time ->
          ( Model time interval running currentTime, Cmd.none )
        SetInterval intr ->
            case intr of
                Pomodoro ->
                    ( Model pomodoroTime Pomodoro False, Cmd.none )

                Short ->
                    ( Model shortTime Short False, Cmd.none )

                Long ->
                    ( Model longTime Long False, Cmd.none )

        Start ->
            ( Model timeRemaining interval True, Cmd.none )

        Stop ->
            ( Model timeRemaining interval False, Cmd.none )

        Tick time ->
            case running of
                True ->
                    ( Model startTime interval running time, Cmd.none )

                False ->
                    ( Model startTime interval running time, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model.timeRemaining) ]
        , button [ onClick Start ] [ text "Start" ]
        , button [ onClick Stop ] [ text "Stop"]
        , button [ onClick (SetInterval Short)] [ text "Short"]
        ]
