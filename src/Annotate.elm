module Annotate exposing
    ( Config, configure
    , Model, init
    , Msg, update, keysChanged
    , view
    , subscriptions
    )

{-| An opinionated annotation application.


# Config

@docs Config, configure


# Model

@docs Model, init


# Update

@docs Msg, update, keysChanged


# View

@docs view


# Subscriptions

@docs subscriptions

-}

import Annotate.Controls as Controls
import Annotate.EditState as EditState exposing (AnnotationConfig, EditState, Finish(..))
import Annotate.Environment exposing (Environment, OperatingSystem(..))
import Annotate.Image as Image exposing (Image)
import Annotation exposing (Annotation, Choice(..))
import Annotation.Options exposing (StrokeStyle(..))
import Annotation.Vertices exposing (Vertex(..))
import Array exposing (Array)
import AutoExpand
import Browser.Dom as Dom
import DrawingArea.Definitions as Definitions
import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, style, title)
import Html.Events exposing (onClick, onMouseDown)
import Icons as Icons
import Keyboard exposing (Key(..), KeyChange(..), anyKeyUpper)
import List.Extra
import Position exposing (Position, StartPosition)
import Svg exposing (Svg, svg)
import Svg.Attributes as Attr
import Svg.Lazy as Svg
import Task
import UndoList exposing (UndoList)
import Utils exposing (isSpotlight, mapAtIndex, removeItem)


{-| Configuration for your annotator.
-}
type Config
    = Config
        { environment : Environment
        , image : Image
        , pressedKeys : List Key
        }


type alias AnnotationMenu =
    { index : Maybe Int
    , position : Position
    }


{-| -}
type alias Model =
    { -- Annotation Editing State
      edits : UndoList (Array Annotation)
    , editState : EditState
    , clipboard : Maybe Annotation

    -- Control UI State
    , controls : Controls.State

    -- Image Annotator Modals
    , annotationMenu : Maybe AnnotationMenu
    }


{-| The Annotator does not need to manipulate these values, but they are essential for the following features:

1.  Providing OS-specific hotkeys
2.  Determining drawing bounds
3.  Calculating SVG masks
4.  Responding to keyboard interactions

-}
configure :
    { environment : Environment
    , image : Image
    , pressedKeys : List Key
    }
    -> Config
configure { environment, image, pressedKeys } =
    Config { environment = environment, image = image, pressedKeys = pressedKeys }


{-| -}
init : Environment -> ( Model, Cmd msg )
init environment =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { edits = UndoList.fresh Array.empty
    , editState = EditState.initialState
    , clipboard = Nothing
    , controls = Controls.initialState
    , annotationMenu = Nothing
    }


type SelectedMsg
    = FocusTextArea
    | SelectText
    | StartEditingText
    | PreventTextMouseDown
    | TextBoxInput { textValue : String, state : AutoExpand.State }
    | FinishEditingText
      -- Edit Updates
    | SelectAndMoveAnnotation StartPosition
      -- Move updates
    | StartMovingAnnotation StartPosition
      -- Resize updates
    | StartResizingAnnotation Vertex StartPosition
    | FinishedEdit
    | ToggleSelectedAnnotationMenu Position
    | BringAnnotationToFront
    | SendAnnotationToBack


{-| -}
type Msg
    = StartDrawing Position
    | ContinueDrawing Position
    | MoveAnnotation Position
    | ResizeAnnotation Position
    | ResetToReadyToDraw
    | ControlsUpdate Controls.Msg
    | GotSelectedMsg Int SelectedMsg
    | ToggleAnnotationMenu Position
      -- History updates
    | Undo
    | Redo
    | Save
      -- Modal updates
    | CloseAllMenus
    | LogError String


alterControls : (Controls.State -> Controls.State) -> Model -> Model
alterControls fn model =
    { model | controls = fn model.controls }


updateSelected : Config -> Annotation -> SelectedMsg -> Model -> ( Model, Cmd Msg )
updateSelected config annotation msg model =
    case msg of
        FocusTextArea ->
            ( startEditingText model
            , textAreaDomId annotation.id
                |> Dom.focus
                |> Task.attempt (tryToEdit annotation.id)
            )

        SelectText ->
            ( model
            , Cmd.none
              --selectText (textAreaDomId annotation.id)
            )

        StartEditingText ->
            ( model
                |> alterControls Controls.closeDropdown
            , Cmd.none
              -- selectText (textAreaDomId annotation.id)
            )

        PreventTextMouseDown ->
            ( model
            , Cmd.none
            )

        TextBoxInput { state, textValue } ->
            ( model
                |> editTextBoxAnnotation annotation state textValue
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        FinishEditingText ->
            finishEdit config annotation model

        SelectAndMoveAnnotation start ->
            ( model
                |> selectAnnotation annotation.id
                |> startMovingAnnotation start
            , Cmd.none
            )

        StartMovingAnnotation start ->
            ( model
                |> selectAnnotation annotation.id
                |> startMovingAnnotation start
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        StartResizingAnnotation vertex start ->
            ( model
                |> startResizingAnnotation annotation vertex start
                |> resizeAnnotation annotation start
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        FinishedEdit ->
            finishEdit config annotation model

        BringAnnotationToFront ->
            ( bringAnnotationToFront annotation model
            , Cmd.none
            )

        SendAnnotationToBack ->
            ( sendAnnotationToBack annotation model
            , Cmd.none
            )

        ToggleSelectedAnnotationMenu pos ->
            ( model
                |> toggleAnnotationMenu (Just annotation.id) pos
                |> selectAnnotation annotation.id
            , Cmd.none
            )


{-| -}
update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update ((Config { environment, image, pressedKeys }) as config) msg model =
    case msg of
        StartDrawing pos ->
            ( model
                |> startDrawing config pos
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        ContinueDrawing pos ->
            ( model
                |> findAnnotation
                |> Maybe.map (\annotation -> continueDrawing pos annotation model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        MoveAnnotation pos ->
            ( model
                |> findAnnotation
                |> Maybe.map (\annotation -> moveAnnotation pos annotation model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        ResizeAnnotation pos ->
            ( model
                |> findAnnotation
                |> Maybe.map (\annotation -> resizeAnnotation annotation pos model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        GotSelectedMsg id selectedMsg ->
            case Array.get id model.edits.present of
                Just annotation ->
                    updateSelected config annotation selectedMsg model

                Nothing ->
                    ( model, Cmd.none )

        CloseAllMenus ->
            ( model
                |> closeAllMenus
            , Cmd.none
            )

        ResetToReadyToDraw ->
            ( model
                |> resetEditState
            , Cmd.none
            )

        ControlsUpdate controlsMsg ->
            let
                newState =
                    Controls.update controlsMsg model.controls
            in
            ( model
                |> updateAnySelectedAnnotations (Annotation.setStyles newState.annotationStyles)
                |> alterControls (always newState)
            , Cmd.none
            )

        ToggleAnnotationMenu pos ->
            ( model
                |> toggleAnnotationMenu Nothing pos
            , Cmd.none
            )

        Undo ->
            ( undoEdit model
            , Cmd.none
            )

        Redo ->
            ( redoEdit model
            , Cmd.none
            )

        Save ->
            ( model
                |> resetEditState
            , Cmd.none
              -- exportToImage image.id
            )

        LogError error ->
            ( model
            , Cmd.none
              -- Log.error error
            )


{-| Do not add this annotations array change to undo history
-}
skipChange : Model -> Array Annotation -> Model
skipChange model annotations =
    { model | edits = UndoList.mapPresent (always annotations) model.edits }


resetEditState : Model -> Model
resetEditState model =
    { model | editState = EditState.initialState }


finishEdit : Config -> Annotation -> Model -> ( Model, Cmd Msg )
finishEdit config annotation model =
    case EditState.finish (annotationConfig config model annotation.id) annotation model.editState of
        Successful newState updatedAnnotation ->
            finishValidDrawing updatedAnnotation { model | editState = newState }

        Unsuccessful newState ->
            ( { model | editState = newState, edits = UndoList.mapPresent (removeItem annotation.id) model.edits }
            , Cmd.none
            )

        InvalidTransition _ ->
            ( model, Cmd.none )



-- DRAWING


shouldSnap : Config -> Bool
shouldSnap (Config { pressedKeys }) =
    List.member Shift pressedKeys


startDrawing : Config -> StartPosition -> Model -> Model
startDrawing config start model =
    let
        numAnnotations =
            Array.length model.edits.present

        styles =
            model.controls.annotationStyles

        id =
            numAnnotations

        drawing =
            { id = id
            , choice = model.controls.annotation
            , start = start
            , end = start
            , styles = styles
            }
    in
    case EditState.startDrawing (annotationConfig config model id) drawing model.editState of
        Ok ( newEditState, annotation ) ->
            { model
                | editState = newEditState
            }
                |> addAnnotation annotation

        Err _ ->
            model


continueDrawing : Position -> Annotation -> Model -> Model
continueDrawing pos annotation model =
    case EditState.continueDrawing pos annotation model.editState of
        Ok ( newEditState, updatedAnnotation ) ->
            { model
                | editState = newEditState
                , edits =
                    UndoList.mapPresent (Array.set annotation.id updatedAnnotation) model.edits
            }

        Err _ ->
            model


textAreaDomId : Int -> String
textAreaDomId id =
    "text-box-edit--" ++ String.fromInt id


selectText : Int -> Result Dom.Error () -> Msg
selectText index result =
    case result of
        Ok _ ->
            GotSelectedMsg index SelectText

        Err error ->
            LogError (domErrToString error)


finishValidDrawing : Annotation -> Model -> ( Model, Cmd Msg )
finishValidDrawing annotation model =
    let
        numAnnotations =
            Array.length model.edits.present
    in
    ( { model | edits = UndoList.mapPresent (Array.set annotation.id annotation) model.edits }
    , case annotation.choice of
        TextBox ->
            textAreaDomId (numAnnotations - 1)
                |> Dom.focus
                |> Task.attempt (selectText (numAnnotations - 1))

        _ ->
            Cmd.none
    )


updateAnySelectedAnnotations : (Annotation -> Annotation) -> Model -> Model
updateAnySelectedAnnotations fn model =
    case EditState.selected model.editState of
        Just index ->
            { model
                | edits = UndoList.new (mapAtIndex index fn model.edits.present) model.edits
            }

        Nothing ->
            model


selectAnnotation : Int -> Model -> Model
selectAnnotation index model =
    model.edits.present
        |> Array.get index
        |> Maybe.andThen (\annotation -> Result.toMaybe <| EditState.select annotation model.editState)
        |> Maybe.map (\newEditState -> { model | editState = newEditState })
        |> Maybe.withDefault model


addAnnotation : Annotation -> Model -> Model
addAnnotation annotation model =
    { model
        | edits = UndoList.new (Array.push annotation model.edits.present) model.edits
    }


startEditingText : Model -> Model
startEditingText model =
    EditState.startTyping model.editState
        |> Result.map (\newState -> { model | editState = newState })
        |> Result.withDefault model


editTextBoxAnnotation : Annotation -> AutoExpand.State -> String -> Model -> Model
editTextBoxAnnotation annotation autoExpandState autoExpandText model =
    model.edits.present
        |> mapAtIndex annotation.id (Annotation.updateTextArea autoExpandState autoExpandText)
        |> skipChange model


startMovingAnnotation : Position -> Model -> Model
startMovingAnnotation newPos model =
    case EditState.startMoving newPos model.editState of
        Ok newEditState ->
            { model | editState = newEditState }

        Err _ ->
            model


moveAnnotation : Position -> Annotation -> Model -> Model
moveAnnotation newPos annotation model =
    case EditState.continueMoving newPos annotation model.editState of
        Ok ( newEditState, updatedAnnotation ) ->
            { model | editState = newEditState, edits = UndoList.mapPresent (Array.set annotation.id updatedAnnotation) model.edits }

        Err _ ->
            model


startResizingAnnotation : Annotation -> Vertex -> StartPosition -> Model -> Model
startResizingAnnotation annotation vertex start model =
    EditState.startResizing start vertex (Annotation.startAndEnd annotation) model.editState
        |> Result.map (\newEditState -> { model | editState = newEditState })
        |> Result.withDefault model


resizeAnnotation : Annotation -> Position -> Model -> Model
resizeAnnotation annotation curPos model =
    case EditState.continueResizing curPos annotation model.editState of
        Ok ( newEditState, updatedAnnotation ) ->
            { model
                | edits = UndoList.mapPresent (Array.set annotation.id updatedAnnotation) model.edits
                , editState = newEditState
            }

        Err _ ->
            model


copySelectedAnnotation : Annotation -> Model -> Model
copySelectedAnnotation annotation model =
    { model | clipboard = Just { annotation | id = Array.length model.edits.present } }


cutSelectedAnnotation : Annotation -> Model -> Model
cutSelectedAnnotation annotation model =
    { model
        | clipboard = Just annotation
        , edits = UndoList.new (Array.filter ((/=) annotation.id << .id) model.edits.present) model.edits
    }


pasteAnnotation : Model -> Model
pasteAnnotation model =
    case model.clipboard of
        Just annotation ->
            { model
                | edits = UndoList.new (Array.push (Annotation.move ( 10, 10 ) annotation) model.edits.present) model.edits
                , clipboard = Just (Annotation.move ( 10, 10 ) annotation)
            }
                |> selectAnnotation (Array.length model.edits.present)

        Nothing ->
            model


deleteSelectedAnnotation : Annotation -> Model -> Model
deleteSelectedAnnotation annotation model =
    { model
        | edits = UndoList.new (Array.filter ((/=) annotation.id << .id) model.edits.present) model.edits
        , editState = EditState.initialState
    }


undoEdit : Model -> Model
undoEdit model =
    { model | edits = UndoList.undo model.edits }


redoEdit : Model -> Model
redoEdit model =
    { model | edits = UndoList.redo model.edits }


domErrToString : Dom.Error -> String
domErrToString err =
    case err of
        Dom.NotFound str ->
            str


tryToEdit : Int -> Result Dom.Error () -> Msg
tryToEdit index result =
    case result of
        Ok _ ->
            GotSelectedMsg index StartEditingText

        Err err ->
            LogError (domErrToString err)


tryToBlur : Result Dom.Error () -> Msg
tryToBlur result =
    case result of
        Ok _ ->
            ResetToReadyToDraw

        Err err ->
            LogError (domErrToString err)


toggleAnnotationMenu : Maybe Int -> Position -> Model -> Model
toggleAnnotationMenu selectedIndex position model =
    case model.annotationMenu of
        Just menu ->
            if menu.index == selectedIndex then
                { model | annotationMenu = Nothing }

            else
                { model
                    | annotationMenu = Just { index = selectedIndex, position = position }
                }

        Nothing ->
            { model
                | annotationMenu = Just { index = selectedIndex, position = position }
                , editState =
                    case selectedIndex of
                        Just index ->
                            selectAnnotation index model
                                |> .editState

                        Nothing ->
                            model.editState
            }


bringToFront : Int -> Array Annotation -> Array Annotation
bringToFront index annotations =
    case Array.get index annotations of
        Just annotation ->
            Array.push annotation (Array.append (Array.slice 0 index annotations) (Array.slice (index + 1) (Array.length annotations) annotations))

        Nothing ->
            annotations


bringAnnotationToFront : Annotation -> Model -> Model
bringAnnotationToFront annotation model =
    { model
        | edits = UndoList.new (bringToFront annotation.id model.edits.present) model.edits
        , editState = EditState.initialState
    }
        |> closeAllMenus


sendToBack : Int -> Array Annotation -> Array Annotation
sendToBack index annotations =
    case Array.get index annotations of
        Just annotation ->
            Array.append (Array.fromList [ annotation ]) (Array.append (Array.slice 0 index annotations) (Array.slice (index + 1) (Array.length annotations) annotations))

        Nothing ->
            annotations


sendAnnotationToBack : Annotation -> Model -> Model
sendAnnotationToBack annotation model =
    { model
        | edits = UndoList.new (sendToBack annotation.id model.edits.present) model.edits
        , editState = EditState.initialState
    }
        |> closeAllMenus


closeAllMenus : Model -> Model
closeAllMenus model =
    { model | annotationMenu = Nothing, controls = Controls.closeDropdown model.controls }


controlKeys : OperatingSystem -> List Key
controlKeys os =
    case os of
        MacOS ->
            [ Meta, ContextMenu ]

        Windows ->
            [ Control ]


findAnnotation : Model -> Maybe Annotation
findAnnotation model =
    model.editState
        |> EditState.selected
        |> Maybe.andThen (\id -> Array.get id model.edits.present)


keysChanged : Config -> Maybe KeyChange -> Model -> ( Model, Cmd Msg )
keysChanged config maybeKeyChange model =
    case maybeKeyChange of
        Just keyChange ->
            case keyChange of
                KeyDown key ->
                    ( withKeyDown config key (findAnnotation model) model, Cmd.none )

                KeyUp _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model
            , Cmd.none
            )


withControlPressed : Config -> Key -> Maybe Annotation -> Model -> Model
withControlPressed (Config { pressedKeys }) key selected model =
    case ( key, selected ) of
        ( Character "C", Just annotation ) ->
            copySelectedAnnotation annotation model

        ( Character "X", Just annotation ) ->
            cutSelectedAnnotation annotation model

        ( Character "V", _ ) ->
            pasteAnnotation model

        ( Character "Z", _ ) ->
            if List.member Shift pressedKeys then
                redoEdit model

            else
                undoEdit model

        _ ->
            model


withKeyDownHelper : Config -> Key -> Maybe Annotation -> Model -> Model
withKeyDownHelper (Config { pressedKeys }) key selected model =
    case ( key, selected ) of
        ( Escape, _ ) ->
            model
                |> alterControls Controls.closeDropdown
                |> resetEditState

        ( Delete, Just annotation ) ->
            deleteSelectedAnnotation annotation model

        ( Backspace, Just annotation ) ->
            deleteSelectedAnnotation annotation model

        _ ->
            model


withKeyDown : Config -> Key -> Maybe Annotation -> Model -> Model
withKeyDown ((Config { environment, pressedKeys }) as config) key selected model =
    let
        newModel =
            alterControls (Controls.onKeyDown key) model
    in
    if isCtrlPressed pressedKeys environment.operatingSystem then
        withControlPressed config key selected newModel

    else
        withKeyDownHelper config key selected newModel


isCtrlPressed : List Key -> OperatingSystem -> Bool
isCtrlPressed pressedKeys os =
    List.any (\key -> List.member key (Debug.log "pressed" pressedKeys)) (controlKeys os)


viewModals : Model -> Html Msg
viewModals model =
    case model.annotationMenu of
        Just { index, position } ->
            viewAnnotationMenu position index

        Nothing ->
            text ""


viewModalMask : Maybe modal -> Html Msg
viewModalMask modal =
    let
        showingAnyMenu =
            case modal of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    div
        [ classList [ ( "modal-mask", True ), ( "hidden", not showingAnyMenu ) ]
        , onClick CloseAllMenus
        ]
        []


viewPixelatedImage : Image -> Svg Msg
viewPixelatedImage { width, height, url } =
    Svg.image
        [ Attr.width (String.fromInt (round width))
        , Attr.height (String.fromInt (round height))
        , Attr.xlinkHref url
        , Attr.filter "url(#pixelate)"
        ]
        []


viewImage : Image -> Svg Msg
viewImage { url, width, height } =
    Svg.image
        [ Attr.class "image-to-annotate"
        , Attr.width (String.fromInt (round width))
        , Attr.height (String.fromInt (round height))
        , Attr.xlinkHref url
        , Attr.mask "url(#pixelateMask)"
        ]
        []


canvasConfig : EditState.CanvasConfig Msg
canvasConfig =
    EditState.configureCanvas
        { startedDrawing = StartDrawing
        , drew = ContinueDrawing
        , moved = MoveAnnotation
        , resized = ResizeAnnotation
        , clicked = \index -> GotSelectedMsg index FinishEditingText
        , rightClicked = ToggleAnnotationMenu
        , finished = \index -> GotSelectedMsg index FinishedEdit
        }


canvasAttributes : EditState -> List (Svg.Attribute Msg)
canvasAttributes editState =
    [ id "canvas"
    , class "image-edit"
    , Html.Events.onMouseDown CloseAllMenus
    , Html.Attributes.contextmenu "annotation-menu"
    ]
        ++ EditState.canvasEvents canvasConfig editState


viewDrawingArea : Config -> Model -> Html Msg
viewDrawingArea config model =
    div
        (canvasAttributes model.editState)
        [ viewSvgArea config model (Array.toList model.edits.present) ]


withMask : List Annotation -> List (Svg Msg) -> List (Svg Msg)
withMask annotations svgs =
    case List.Extra.findIndex (isSpotlight << .choice) annotations of
        Just index ->
            List.Extra.intercalate [ Definitions.viewMask ] [ List.take index svgs, List.drop index svgs ]

        Nothing ->
            svgs


viewSvgArea : Config -> Model -> List Annotation -> Svg Msg
viewSvgArea ((Config { image }) as config) model annotations =
    let
        svgAnnotations =
            annotations
                |> List.indexedMap (viewAnnotation config model)
                |> withMask annotations

        svgs =
            Svg.lazy viewPixelatedImage image :: Svg.lazy viewImage image :: svgAnnotations
    in
    svg
        [ Attr.id "drawing-area"
        , Attr.class "drawing-area"
        , Attr.width (String.fromInt (round image.width))
        , Attr.height (String.fromInt (round image.height))
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        (Definitions.view (List.indexedMap (viewDef config model) annotations) :: svgs)


viewDef : Config -> Model -> Int -> Annotation -> Annotation.Def Msg
viewDef config model index annotation =
    EditState.viewDefinition (annotationConfig config model index) annotation model.editState


viewAnnotation : Config -> Model -> Int -> Annotation -> Svg Msg
viewAnnotation config model index annotation =
    EditState.viewAnnotation (annotationConfig config model index) annotation model.editState


annotationConfig : Config -> Model -> Int -> AnnotationConfig Msg
annotationConfig config model index =
    EditState.configureAnnotation
        { clicked = GotSelectedMsg index << SelectAndMoveAnnotation
        , rightClicked = GotSelectedMsg index << ToggleSelectedAnnotationMenu
        , moved = GotSelectedMsg index << StartMovingAnnotation
        , resized = \pos vertex -> GotSelectedMsg index (StartResizingAnnotation pos vertex)
        , typed = GotSelectedMsg index << TextBoxInput
        , focused = GotSelectedMsg index FocusTextArea
        , snapped = shouldSnap config
        }


inPixels : Int -> String
inPixels number =
    String.fromInt number ++ "px"


viewAnnotationMenu : Position -> Maybe Int -> Html Msg
viewAnnotationMenu pos selectedIndex =
    div
        [ id "annotation-menu"
        , class "annotation-menu"
        , style "top" (inPixels pos.y)
        , style "left" (inPixels pos.x)
        ]
        [ ul [ class "annotation-menu__list" ]
            (case selectedIndex of
                Just index ->
                    [ viewAnnotationMenuItem (GotSelectedMsg index BringAnnotationToFront) "Bring to Front"
                    , viewAnnotationMenuItem (GotSelectedMsg index SendAnnotationToBack) "Send to Back"
                    ]

                Nothing ->
                    [ viewDisabledAnnotationMenuItem "Bring to Front"
                    , viewDisabledAnnotationMenuItem "Send to Back"
                    ]
            )
        ]


viewDisabledAnnotationMenuItem : String -> Html msg
viewDisabledAnnotationMenuItem buttonText =
    li [ class "annotation-menu__item" ]
        [ button
            [ class "annotation-menu__button"
            , disabled True
            ]
            [ text buttonText ]
        ]


viewAnnotationMenuItem : Msg -> String -> Html Msg
viewAnnotationMenuItem msg buttonText =
    li [ class "annotation-menu__item" ]
        [ button
            [ class "annotation-menu__button"
            , onClick msg
            ]
            [ text buttonText ]
        ]


{-| -}
view : Config -> Model -> Html Msg
view config model =
    div
        [ class "annotation-app" ]
        [ viewModals model
        , viewModalMask model.annotationMenu
        , viewControls config model
        , viewDrawingArea config model
        ]



-- viewNavigationControls : Html Msg
-- viewNavigationControls =
--     div [ class "navigation-controls" ]
--         [ a [ Route.href Route.Gallery, class "cancel-button" ] [ text "Back" ]
--         , button [ onClick Save, class "save-button" ] [ text "Save" ]
--         ]


viewHistoryControls : OperatingSystem -> UndoList a -> Html Msg
viewHistoryControls os edits =
    div [ class "history-controls" ]
        [ button
            [ onClick Undo
            , class "history-button"
            , disabled ((not << UndoList.hasPast) edits)
            , title <|
                case os of
                    MacOS ->
                        "Undo (Cmd-Z)"

                    Windows ->
                        "Undo (Ctrl + Z)"
            ]
            [ Icons.viewUndoArrow ]
        , button
            [ onClick Redo
            , class "history-button flip"
            , disabled ((not << UndoList.hasFuture) edits)
            , title <|
                case os of
                    MacOS ->
                        "Redo (Cmd-Shift-Z)"

                    Windows ->
                        "Redo (Ctrl + Shift + Z)"
            ]
            [ Icons.viewUndoArrow ]
        ]


viewControls : Config -> Model -> Html Msg
viewControls (Config { environment }) model =
    div
        [ class "controls" ]
        [ viewHistoryControls environment.operatingSystem model.edits
        , Html.map ControlsUpdate (Controls.view (controlsConfig model.controls.annotationStyles environment.operatingSystem) model.controls)
        ]


controlsConfig : Annotation.Styles -> OperatingSystem -> Controls.Config
controlsConfig styles os =
    Controls.Config styles os


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ EditState.subscriptions canvasConfig model.editState
        , Sub.map ControlsUpdate (Controls.subscriptions model.controls)
        ]
