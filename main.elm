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

type alias Model = Dict String HabitRecord

startModel = Dict.empty

model : Signal Model
model =
  -- in a real app this foldp would likely be more complicated, using Signal.merge
  -- to combine signals such as user actions.
  -- Signal.map converts a Signal Model -> Signal Action
  Signal.foldp update startModel (Signal.map ServerUpdate serverValue)

-- in a real app, all actions including user actions could be modeled by this Action
-- type that gets passed to the update function
type Action =
  NoOp
  | ServerUpdate Model

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    ServerUpdate newModel -> newModel

viewCheckin : (String, String) -> Html
viewCheckin (dateString, color) =
  li [] [
    text <| dateString ++ ": ",
    b [] [ text color ]
  ]

viewHabitRecord : HabitRecord -> Html
viewHabitRecord { checkins, decayRate } =
  div [] [
    p [] [ text <| "Decay Rate: " ++ toString decayRate ],
    ul [] <| List.map viewCheckin checkins
  ]

view : Model -> Html
view model =
  ul [] <| List.map
    (\(label, habitRecord) -> li [] [
      h1 [] [ text label ],
      viewHabitRecord habitRecord
    ])
    <| Dict.toList model


main : Signal Html
main =
  Signal.map view model
