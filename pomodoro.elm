module Pomodoro exposing (..)

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
      25 * minute
    Short ->
      0.1 * minute
    Long ->
      10 * minute

init : ( Model, Cmd Msg )
init =
    ( Model Pomodoro False 0, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg { interval, running, timeElapsed } =
    case msg of
        SetInterval intr ->
          ( Model intr False 0, Cmd.none)
        Start ->
            ( Model interval True timeElapsed, Cmd.none )
        Stop ->
            ( Model interval False timeElapsed, Cmd.none )

        Tick time ->
            case running of
                True ->
                    ( Model interval running (updateTimeElapsed timeElapsed interval), Cmd.none )

                False ->
                    (Model interval running timeElapsed, Cmd.none )


updateTimeElapsed : Time -> Interval -> Time
updateTimeElapsed timeElapsed interval =
  if timeElapsed == (intervalToTime interval) then
    timeElapsed
  else
    timeElapsed + second

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


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
