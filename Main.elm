module Main exposing (..)

import Html exposing (Html, div, text, button)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Task exposing (..)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { interval : Interval
    , running : Bool
    , timeElapsed : Time
    , previousTick : Time
    }

type Interval
    = Pomodoro
    | Short
    | Long


type Msg
    = SetInterval Interval
    | Start
    | Stop
    | Tick Time


intervalToTime : Interval -> Time
intervalToTime intr =
  case intr of
    Pomodoro ->
      pomodoroTime
    Short ->
      shortTime
    Long ->
      longTime

pomodoroTime =
    25 * minute

shortTime =
    5 * minute

longTime =
    10 * minute

init : ( Model, Cmd Msg )
init =
    ( Model Pomodoro False 0 0, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg { interval, running, timeElapsed, previousTick } =
    case msg of
        SetInterval intr ->
          ( Model intr False 0 previousTick, Cmd.none)
        Start ->
            ( Model interval True timeElapsed previousTick, Cmd.none )
        Stop ->
            ( Model interval False timeElapsed previousTick, Cmd.none )

        Tick time ->
            case running of
                True ->
                    ( Model interval running (updateTimeElapsed timeElapsed time previousTick interval) time, Cmd.none )

                False ->
                    (Model interval running timeElapsed time, Cmd.none )


updateTimeElapsed : Time -> Time -> Time -> Interval -> Time
updateTimeElapsed timeElapsed time previousTick intr =
  if previousTick == 0 || timeElapsed == (intervalToTime intr) then
    timeElapsed
  else
    timeElapsed + (time - previousTick)
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every millisecond Tick


timeRemainingView : Time -> String
timeRemainingView time =
  toString (floor (inMinutes time)) ++ ":" ++ (String.padLeft 2 '0' (toString ( rem (floor (inSeconds time)) 60 )))


view : Model -> Html Msg
view { timeElapsed, interval } =
    div []
        [ div [] [ timeRemainingView ((intervalToTime interval) - timeElapsed) |> text ]
        , button [ onClick Start ] [ text "Start" ]
        , button [ onClick Stop ] [ text "Stop"]
        , button [ onClick (SetInterval Short)] [ text "Short"]
        , button [ onClick (SetInterval Long)] [ text "Long"]
        , button [ onClick (SetInterval Pomodoro)] [ text "Pomodoro"]
        ]
