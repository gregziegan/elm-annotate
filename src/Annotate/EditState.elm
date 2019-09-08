module Annotate.EditState exposing
    ( initialState, EditState
    , configureCanvas, CanvasConfig, configureAnnotation, AnnotationConfig, subscriptions
    , selected
    , startDrawing, continueDrawing
    , select
    , startMoving, continueMoving
    , startResizing, continueResizing
    , startTyping
    , finish, Finish(..)
    , canvasEvents, viewAnnotation, viewDefinition
    )

{-| The finite state machine for annotating.
See <https://github.com/thebritican/goat/wiki/The-Annotation-Editor's-Finite-State-Machine>


# State

@docs initialState, EditState


# Configuration

@docs configureCanvas, CanvasConfig, configureAnnotation, AnnotationConfig, subscriptions


# Query

@docs selected


# Update


## Draw

@docs startDrawing, continueDrawing


## Select

@docs select


## Move

@docs startMoving, continueMoving


## Resize

@docs startResizing, continueResizing


## Type

@docs startTyping


## Transition

@docs finish, Finish


# View

@docs canvasEvents, viewAnnotation, viewDefinition

-}

import Annotation exposing (Annotation, Choice(..))
import Annotation.Vertices exposing (Vertex(..))
import AutoExpand
import Browser.Events as Events
import EventUtils exposing (alwaysPreventDefault, onMouseDown, onMouseUp, stopPropagationAndDefault)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Json
import Keyboard
import Position exposing (EndPosition, Position, StartPosition, calcDistance)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Attr



-- STATE


{-| The current state of editing an image
-}
type EditState
    = NotSelecting
    | Drawing Annotation.Id
    | Selecting Annotation.Id
    | Moving Annotation.Id MovingInfo
    | Resizing Annotation.Id ResizingInfo
    | EditingText Annotation.Id


{-| Make the drawing canvas interactive
-}
configureCanvas :
    { startedDrawing : StartPosition -> msg
    , drew : Position -> msg
    , moved : Position -> msg
    , resized : Position -> msg
    , clicked : Annotation.Id -> msg
    , rightClicked : Position -> msg
    , finished : Annotation.Id -> msg
    }
    -> CanvasConfig msg
configureCanvas { startedDrawing, drew, moved, resized, clicked, rightClicked, finished } =
    CanvasConfig
        { startedDrawing = startedDrawing
        , drew = drew
        , moved = moved
        , resized = resized
        , clicked = clicked
        , rightClicked = rightClicked
        , finished = finished
        }


{-| Individual Annotations (svgs) are moved on mouse/touch interaction.
-}
type AnnotationConfig msg
    = AnnotationConfig
        { clicked : StartPosition -> msg
        , rightClicked : Position -> msg
        , moved : StartPosition -> msg
        , resized : Vertex -> StartPosition -> msg
        , typed : { state : AutoExpand.State, textValue : String } -> msg
        , focused : msg
        , snapped : Bool
        }


{-| This configuration is for the svg drawing area.
-}
type CanvasConfig msg
    = CanvasConfig
        { startedDrawing : StartPosition -> msg
        , drew : Position -> msg
        , resized : Position -> msg
        , moved : Position -> msg
        , clicked : Annotation.Id -> msg
        , rightClicked : Position -> msg
        , finished : Annotation.Id -> msg
        }


type alias MovingInfo =
    { start : Position
    , translate : ( Int, Int )
    }


type alias ResizingInfo =
    { start : Position
    , curPos : Position
    , vertex : Vertex
    , originalCoords : ( Position, Position )
    }


{-| Describes how finishing an edit went
-}
type Finish
    = InvalidTransition String
    | Unsuccessful EditState
    | Successful EditState Annotation


{-| Make annotations interactive
-}
configureAnnotation :
    { clicked : StartPosition -> msg
    , rightClicked : Position -> msg
    , moved : StartPosition -> msg
    , resized : Vertex -> StartPosition -> msg
    , typed : { state : AutoExpand.State, textValue : String } -> msg
    , focused : msg
    , snapped : Bool
    }
    -> AnnotationConfig msg
configureAnnotation { clicked, rightClicked, moved, resized, typed, focused, snapped } =
    AnnotationConfig
        { clicked = clicked
        , rightClicked = rightClicked
        , moved = moved
        , resized = resized
        , typed = typed
        , focused = focused
        , snapped = snapped
        }


{-| Start with no selections
-}
initialState : EditState
initialState =
    NotSelecting


errorMessage : EditState -> String
errorMessage editState =
    editStateToString editState ++ " is not a valid state to start this transition."


{-| -}
startDrawing :
    AnnotationConfig msg
    ->
        { id : Annotation.Id
        , choice : Annotation.Choice
        , start : StartPosition
        , end : EndPosition
        , styles : Annotation.Styles
        }
    -> EditState
    -> Result String ( EditState, Annotation )
startDrawing config drawing editState =
    let
        annotation =
            Annotation.init (configForAnnotation config drawing.id editState) drawing
    in
    case editState of
        NotSelecting ->
            Ok ( Drawing drawing.id, annotation )

        Selecting _ ->
            Ok ( Drawing drawing.id, annotation )

        _ ->
            Err (errorMessage editState)


{-| -}
continueDrawing : Position -> Annotation -> EditState -> Result String ( EditState, Annotation )
continueDrawing pos annotation editState =
    case editState of
        Drawing _ ->
            case annotation.choice of
                FreeHand ->
                    Ok ( editState, { annotation | end = pos, positions = pos :: annotation.positions } )

                _ ->
                    Ok ( editState, { annotation | end = pos } )

        _ ->
            Err (errorMessage editState)


editStateToString : EditState -> String
editStateToString editState =
    case editState of
        NotSelecting ->
            "NotSelecting"

        Drawing _ ->
            "Drawing"

        Selecting _ ->
            "Selecting"

        Moving _ _ ->
            "Moving"

        Resizing _ _ ->
            "Resizing"

        EditingText _ ->
            "EditingText"


sumDistance : Position -> ( Float, Position ) -> ( Float, Position )
sumDistance position ( distance, previousPosition ) =
    ( distance
        + calcDistance position previousPosition
    , position
    )


finishDrawingFreeHand : Annotation -> Finish
finishDrawingFreeHand annotation =
    let
        ( totalDistance, _ ) =
            List.foldl sumDistance ( 0.0, annotation.start ) (annotation.positions ++ [ annotation.end ])
    in
    if totalDistance < minDrawingDistance then
        Unsuccessful NotSelecting

    else
        Successful NotSelecting annotation


finishDrawingSpotlight : Annotation -> Finish
finishDrawingSpotlight annotation =
    if calcDistance annotation.start annotation.end > minSpotlightDrawingDistance then
        Successful NotSelecting annotation

    else
        Unsuccessful NotSelecting


finishDrawingSvg : Annotation -> Finish
finishDrawingSvg annotation =
    if calcDistance annotation.start annotation.end > minDrawingDistance then
        Successful NotSelecting annotation

    else
        Unsuccessful NotSelecting


{-| -}
finish : AnnotationConfig msg -> Annotation -> EditState -> Finish
finish config annotation editState =
    case editState of
        NotSelecting ->
            InvalidTransition (errorMessage editState)

        Drawing _ ->
            case annotation.choice of
                FreeHand ->
                    finishDrawingFreeHand annotation

                TextBox ->
                    Successful (EditingText annotation.id) annotation

                RectangleSpotlight ->
                    finishDrawingSpotlight annotation

                RoundedRectangleSpotlight ->
                    finishDrawingSpotlight annotation

                EllipseSpotlight ->
                    finishDrawingSpotlight annotation

                _ ->
                    finishDrawingSvg annotation

        Selecting _ ->
            InvalidTransition (errorMessage editState)

        Moving id { translate } ->
            Successful (Selecting id) (Annotation.move translate annotation)

        Resizing id resizingInfo ->
            Successful (Selecting id) (resize (configForAnnotation config annotation.id editState).snap resizingInfo annotation)

        EditingText _ ->
            Successful NotSelecting annotation


{-| -}
select : Annotation -> EditState -> Result String EditState
select annotation editState =
    case editState of
        NotSelecting ->
            Ok (Selecting annotation.id)

        Selecting _ ->
            Ok (Selecting annotation.id)

        Moving _ _ ->
            Ok (Selecting annotation.id)

        Resizing _ _ ->
            Ok (Selecting annotation.id)

        _ ->
            Err (errorMessage editState)


{-| -}
startMoving : Position -> EditState -> Result String EditState
startMoving start editState =
    case editState of
        Selecting annotation ->
            Ok (Moving annotation (MovingInfo start ( 0, 0 )))

        _ ->
            Err (errorMessage editState)


{-| -}
continueMoving : Position -> Annotation -> EditState -> Result String ( EditState, Annotation )
continueMoving newPos annotation editState =
    case editState of
        Moving id movingInfo ->
            let
                ( dx, dy ) =
                    ( newPos.x - movingInfo.start.x, newPos.y - movingInfo.start.y )
            in
            Ok ( Moving id { movingInfo | translate = ( dx, dy ) }, annotation )

        _ ->
            Err (errorMessage editState)


{-| -}
startResizing : Position -> Vertex -> ( Position, Position ) -> EditState -> Result String EditState
startResizing start vertex originalCoords editState =
    case editState of
        Selecting id ->
            Ok (Resizing id (ResizingInfo start start vertex originalCoords))

        _ ->
            Err (errorMessage editState)


{-| -}
continueResizing : Position -> Annotation -> EditState -> Result String ( EditState, Annotation )
continueResizing curPos annotation editState =
    case editState of
        Resizing id resizingData ->
            Ok ( Resizing id { resizingData | curPos = curPos }, annotation )

        _ ->
            Err (errorMessage editState)


{-| -}
startTyping : EditState -> Result String EditState
startTyping editState =
    case editState of
        Drawing id ->
            Ok (EditingText id)

        Selecting id ->
            Ok (EditingText id)

        _ ->
            Err (errorMessage editState)


{-| The sidebar is a hard-coded width. This offset is used to shift the incoming mouse position.
TODO: investigate whether this can be skipped by using position: relative, or some
other CSS rule.
-}
controlUIWidth : number
controlUIWidth =
    83


toDrawingPosition : Position -> Position
toDrawingPosition mouse =
    { mouse | x = mouse.x - controlUIWidth, y = mouse.y - 10 }


{-| Global mouse and keyboard interactions used to create and edit annotations
-}
subscriptions : CanvasConfig msg -> EditState -> Sub msg
subscriptions (CanvasConfig config) editState =
    Sub.batch <|
        case editState of
            NotSelecting ->
                []

            Selecting _ ->
                []

            Drawing id ->
                [ Events.onMouseMove (Json.map (config.drew << toDrawingPosition) Position.decoder)
                , Events.onMouseUp (Json.succeed (config.finished id))
                ]

            Moving id _ ->
                [ Events.onMouseMove (Json.map (config.moved << toDrawingPosition) Position.decoder)
                , Events.onMouseUp (Json.succeed (config.finished id))
                ]

            Resizing id _ ->
                [ Events.onMouseMove (Json.map (config.resized << toDrawingPosition) Position.decoder)
                , Events.onMouseUp (Json.succeed (config.finished id))
                ]

            EditingText id ->
                [ Events.onMouseDown (Json.succeed (config.clicked id)) ]


annotationEvents : AnnotationConfig msg -> EditState -> List (Attribute msg)
annotationEvents (AnnotationConfig config) editState =
    case editState of
        NotSelecting ->
            [ stopPropagationAndDefault "mousedown" (Json.map (config.clicked << toDrawingPosition) Position.decoder)
            , Attr.class "pointerCursor"
            , stopPropagationAndDefault "contextmenu" (Json.map config.rightClicked Position.decoder)
            ]

        Drawing _ ->
            [ Attr.class "crosshairCursor" ]

        Selecting _ ->
            [ Attr.class "moveCursor"
            , stopPropagationAndDefault "mousedown" (Json.map (config.moved << toDrawingPosition) Position.decoder)
            , stopPropagationAndDefault "contextmenu" (Json.map config.rightClicked Position.decoder)
            ]

        Moving _ { translate } ->
            let
                ( dx, dy ) =
                    translate
            in
            [ Attr.class "moveCursor"
            , Attr.transform <| "translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ")"
            ]

        Resizing _ _ ->
            [ Attr.class "resizeCursor" ]

        EditingText _ ->
            [ Attr.class "crosshairCursor" ]


vertexEvents : AnnotationConfig msg -> Maybe ( Int, Int ) -> Vertex -> List (Svg.Attribute msg)
vertexEvents (AnnotationConfig config) moving vertex =
    (stopPropagationOn "mousedown" <|
        Json.map (alwaysPreventDefault << config.resized vertex << toDrawingPosition) Position.decoder
    )
        :: (case moving of
                Just translate ->
                    vertexAttrsWhenMoving translate

                Nothing ->
                    []
           )


vertexAttrsWhenMoving : ( Int, Int ) -> List (Attribute msg)
vertexAttrsWhenMoving ( dx, dy ) =
    [ Attr.transform <| "translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ")" ]


{-| Use these events to turn any element into a drawing canvas
-}
canvasEvents : CanvasConfig msg -> EditState -> List (Attribute msg)
canvasEvents (CanvasConfig config) editState =
    let
        listenForContextMenu =
            stopPropagationAndDefault "contextmenu" (Json.map config.rightClicked Position.decoder)

        listenForDrawing =
            onMouseDown <| Json.map (config.startedDrawing << toDrawingPosition) Position.decoder
    in
    case editState of
        NotSelecting ->
            [ listenForDrawing ]

        Selecting _ ->
            [ listenForDrawing ]

        EditingText _ ->
            [ listenForContextMenu
            , Attr.style "cursor: default;"
            ]

        _ ->
            [ listenForContextMenu ]


resize : Bool -> ResizingInfo -> Annotation -> Annotation
resize snap { curPos, vertex, originalCoords } annotation =
    let
        ( start, end ) =
            originalCoords

        constrain =
            Annotation.resizeFn snap annotation
    in
    case vertex of
        Start ->
            { annotation | start = constrain annotation.end curPos }

        End ->
            { annotation | end = constrain annotation.start curPos }

        StartPlusX ->
            { annotation | start = constrain annotation.end curPos, end = Position start.x end.y }

        StartPlusY ->
            { annotation | start = constrain annotation.end curPos, end = Position end.x start.y }


viewHelper : (Annotation.Config msg -> Annotation -> view) -> AnnotationConfig msg -> Annotation -> EditState -> view
viewHelper render config annotation editState =
    let
        attrs =
            configForAnnotation config annotation.id editState
    in
    case editState of
        NotSelecting ->
            render attrs annotation

        Drawing _ ->
            render attrs annotation

        Selecting _ ->
            render attrs annotation

        Moving _ _ ->
            render attrs annotation

        Resizing id resizingInfo ->
            if id == annotation.id then
                render attrs (resize (configForAnnotation config annotation.id editState).snap resizingInfo annotation)

            else
                render attrs annotation

        EditingText _ ->
            render attrs annotation


configForAnnotation : AnnotationConfig msg -> Annotation.Id -> EditState -> Annotation.Config msg
configForAnnotation config id editState =
    let
        (AnnotationConfig { typed, focused, snapped }) =
            config

        defaultConfig =
            { onInput = typed
            , onFocus = focused
            , eventsForVertex = Nothing
            , translate = ( 0, 0 )
            , snap = snapped
            , events = []
            }

        eventsForVertex translate =
            vertexEvents config translate

        static =
            { defaultConfig | snap = False }

        interactive =
            { defaultConfig | events = annotationEvents config editState }

        moving translate =
            { defaultConfig | events = annotationEvents config editState, translate = translate }
    in
    case editState of
        NotSelecting ->
            Annotation.configure { interactive | snap = False }

        Drawing selectedId ->
            if id == selectedId then
                Annotation.configure interactive

            else
                Annotation.configure static

        Selecting selectedId ->
            if id == selectedId then
                interactive
                    |> Annotation.configure
                    |> Annotation.withVertices (eventsForVertex Nothing)

            else
                Annotation.configure static

        Moving selectedId { translate } ->
            if id == selectedId then
                moving translate
                    |> Annotation.configure
                    |> Annotation.withVertices (eventsForVertex (Just translate))

            else
                Annotation.configure static

        Resizing selectedId _ ->
            if id == selectedId then
                interactive
                    |> Annotation.configure
                    |> Annotation.withVertices (eventsForVertex Nothing)

            else
                Annotation.configure static

        EditingText selectedId ->
            if id == selectedId then
                Annotation.configure interactive

            else
                Annotation.configure static


{-| Render an annotation's definition. Please place in the nearest SVG <defs /> tag.
-}
viewDefinition : AnnotationConfig msg -> Annotation -> EditState -> Annotation.Def msg
viewDefinition =
    viewHelper Annotation.viewDefinition


{-| Render an annotation
-}
viewAnnotation : AnnotationConfig msg -> Annotation -> EditState -> Svg msg
viewAnnotation =
    viewHelper Annotation.view


minDrawingDistance : number
minDrawingDistance =
    8


minSpotlightDrawingDistance : number
minSpotlightDrawingDistance =
    12


{-| If there's a selected annotation, return its id.
-}
selected : EditState -> Maybe Annotation.Id
selected editState =
    case editState of
        Drawing id ->
            Just id

        Selecting id ->
            Just id

        Resizing id _ ->
            Just id

        Moving id _ ->
            Just id

        EditingText id ->
            Just id

        _ ->
            Nothing
