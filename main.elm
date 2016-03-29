import Task exposing (Task)
import Html exposing (..)
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import Dict exposing (Dict)
import ElmFire
import ElmFire.Dict
import ElmFire.Op

url = "https://elmfire-extra-hello-world.firebaseio.com/example2/habitRecords"

type alias HabitRecord = {
  checkins: List (String, String),
  decayRate: Int
}

encodeCheckin (dateString, color) =
  (dateString, JE.string color)

config = {
    location = ElmFire.fromUrl url,
    orderOptions = ElmFire.noOrder,
    encoder =
      \item -> JE.object [
        ("checkins", JE.object (List.map encodeCheckin item.checkins)),
        ("decayRate", JE.int item.decayRate)
      ],
    decoder =
      ( JD.object2 HabitRecord
        ("checkins" := JD.keyValuePairs JD.string)
        ("decayRate" := JD.int)
      )
  }

-- mirror the firebase data through a task and signal of a dict
(task, serverValue) = ElmFire.Dict.mirror config

-- you have to run the task so that messages from firebase come through the serverValue signal
port runTask : Task ElmFire.Error (Task ElmFire.Error ())
port runTask = task

type alias Model = String
type alias FirebaseModel = Dict String HabitRecord

startModel = "Waiting..."

model : Signal Model
model =
  -- in a real app this foldp would likely be more complicated, using Signal.merge
  -- to combine signals such as user actions.
  -- Signal.map converts a Signal FirebaseModel -> Signal Action
  Signal.foldp update startModel (Signal.map ServerUpdate serverValue)

-- in a real app, all actions including user actions could be modeled by this Action
-- type that gets passed to the update function
type Action =
  NoOp
  | ServerUpdate FirebaseModel

update : Action -> Model -> Model
update action model =
  let
    -- dictionary reducer function; concatenate key-value list
    showCheckins : List (String, String) -> String
    showCheckins checkins =
      List.foldl (\(dateString, color) accum -> accum ++ ", " ++ dateString ++ "=" ++  color) "" checkins

    showRecord habitRecord =
      "Decay Rate: " ++ (toString habitRecord.decayRate) ++ ". " ++ showCheckins habitRecord.checkins

    reduceModelToString label habitRecord accum =
      case accum of
        "" -> label ++ ": " ++ (showRecord habitRecord)
        _ -> accum ++ ", " ++ label ++ ": " ++ (showRecord habitRecord)
  in
    case action of
      NoOp -> model
      ServerUpdate dict -> Dict.foldl reduceModelToString "" dict

view : Model -> Html
view value =
  p [] [ text value ]

main : Signal Html
main =
  Signal.map view model
