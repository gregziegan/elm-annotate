# elm-annotate

WIP so Elm apps can use image annotation features.

## Tutorial

### Get an opinionated annotator up and running

```elm
import Annotate
import Annotate.Environment
import Environment exposing (Environment, OperatingSystem(..))
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Image exposing (Image)


type alias Model =
    { annotate : Annotate.Model -- elm-annotate's state
    }


{- Map your app's own representation of the runtime environment. elm-annotate does not want to assume
   your platform using commands.
   Please specify an operating system so that tooltips and keyboard interactions are compatible.
 -}
toAnnotateEnv : Environment -> Annotate.Environment.Environment
toAnnotateEnv env =
    case env.operatingSystem of
        Windows ->
            { operatingSystem = Annotate.Environment.Windows }

        MacOS ->
            { operatingSystem = Annotate.Environment.MacOS }


init : Session -> Environment -> ( Model, Cmd msg )
init session environment =
    let
        ( annotate, _ ) =
            Annotate.init (toAnnotateEnv environment)
    in
    ( { annotate = annotate
      , session = session
      }
    , Cmd.none
    )


type Msg
    = SetAnnotateState Annotate.Msg


update : Environment -> Image -> Msg -> Model -> ( Model, Cmd Msg )
update env image msg model =
    case msg of
        SetAnnotateState annotateMsg ->
            let
                ( newState, _ ) =
                    Annotate.update (toAnnotateEnv env) image annotateMsg model.annotate
            in
            ( { model | annotate = newState }
            , Cmd.none
            )


view : Environment -> Image -> Model -> Html Msg
view environment image model =
  div [ class "your-app" ]
      [ Html.map SetAnnotateState (Annotate.view (toAnnotateEnv environment) image model.annotate)
      ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SetAnnotateState (Annotate.subscriptions model.annotate)
        ]
```

## Hook into important events

(under construction)

## Go completely custom

Warning: this is a lot more work! You will need to manage your own undo history, annotation collection, and state
transitions. If we find intermediate stages, we'll make sure to provide an abstraction with some good defaults.

## Development

1. Clone the repo
2. Point your app's `elm.json` => `source_directories` to the repo.

Example:

```json
"source-directories": [
    "./src",
    "../elm-annotate/src"
]
```
