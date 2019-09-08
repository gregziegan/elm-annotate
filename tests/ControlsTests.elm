module ControlsTests exposing (suite)

import Annotate.Controls as Controls exposing (Dropdown, DropdownTrigger(..), DropdownType(..), Msg(..), State, initialState)
import Annotation exposing (Choice(..))
import Annotation.Options as Options exposing (Fill, FontSize, StrokeColor, StrokeStyle(..))
import Color exposing (Color)
import Expect exposing (Expectation)
import Fuzz exposing (int)
import Helpers.Controls exposing (..)
import Keyboard exposing (Key(..))
import Palette
import Test exposing (..)


suite : Test
suite =
    describe "Controls module"
        [ describe "initialState"
            [ test "selects an arrow" <|
                \_ ->
                    Expect.equal Arrow initialState.annotation
            , test "has a one-click rounded rectangle" <|
                \_ ->
                    Expect.equal RoundedRectangle initialState.shape
            , test "has a one-click rounded rectangle spotlight" <|
                \_ ->
                    Expect.equal RoundedRectangleSpotlight initialState.spotlight
            , test "has no selected dropdown" <|
                \_ ->
                    Expect.equal Nothing initialState.currentDropdown
            , test "has default annotation styles" <|
                \_ ->
                    Expect.equal Annotation.defaultStyles initialState.annotationStyles
            ]
        , describe "closeDropdown"
            [ test "unselects dropdown" <|
                \_ ->
                    initialState
                        |> withDropdown shapesDropdown
                        |> Controls.closeDropdown
                        |> .currentDropdown
                        |> Expect.equal Nothing
            ]
        , describe "onKeyDown"
            [ describe "when A is pressed"
                [ test "the arrow is selected" <|
                    \_ ->
                        { initialState | annotation = FreeHand }
                            |> Controls.onKeyDown (key "A")
                            |> .annotation
                            |> Expect.equal Arrow
                ]
            , describe "when C is pressed"
                [ test "selects the rounded rectangle spotlight" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "C")
                            |> .annotation
                            |> Expect.equal RoundedRectangleSpotlight
                , test "selects the rounded rectangle as the new spotlight dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "C")
                            |> .spotlight
                            |> Expect.equal RoundedRectangleSpotlight
                ]
            , describe "when H is pressed"
                [ test "selects free hand" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "H")
                            |> .annotation
                            |> Expect.equal FreeHand
                ]
            , describe "when L is pressed"
                [ test "selects the line" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "L")
                            |> .annotation
                            |> Expect.equal StraightLine
                ]
            , describe "when R is pressed" <|
                [ test "selects the rectangle" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "R")
                            |> .annotation
                            |> Expect.equal Rectangle
                , test "selects the rectangle as the new shape dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "R")
                            |> .shape
                            |> Expect.equal Rectangle
                ]
            , describe "when O is pressed"
                [ test "selects the rounded rectangle" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "O")
                            |> .annotation
                            |> Expect.equal RoundedRectangle
                , test "selects the rounded rectangle as the new shape dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "O")
                            |> .shape
                            |> Expect.equal RoundedRectangle
                ]
            , describe "when E is pressed"
                [ test "selects the ellipse" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "E")
                            |> .annotation
                            |> Expect.equal Ellipse
                , test "selects the ellipse as the new shape dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "E")
                            |> .shape
                            |> Expect.equal Ellipse
                ]
            , describe "when T is pressed"
                [ test "selects the text box" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "T")
                            |> .annotation
                            |> Expect.equal TextBox
                ]
            , describe "when G is pressed"
                [ test "selects the rectangle spotlight" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "G")
                            |> .annotation
                            |> Expect.equal RectangleSpotlight
                , test "selects the rectangle as the new spotlight dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "G")
                            |> .spotlight
                            |> Expect.equal RectangleSpotlight
                ]
            , describe "when I is pressed"
                [ test "selects the ellipse spotlight" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "I")
                            |> .annotation
                            |> Expect.equal EllipseSpotlight
                , test "selects the ellipse as the new spotlight dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "I")
                            |> .spotlight
                            |> Expect.equal EllipseSpotlight
                ]
            , describe "when P is pressed"
                [ test "selects pixelate" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "P")
                            |> .annotation
                            |> Expect.equal Pixelate
                ]
            , describe "when N is pressed"
                [ describe "and the fonts dropdown is open"
                    [ test "closes the dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown Fonts)
                                |> Controls.onKeyDown (key "N")
                                |> .currentDropdown
                                |> Expect.equal Nothing
                    ]
                , describe "and the fonts dropdown is closed"
                    [ test "opens the fonts dropdown" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "N")
                                |> .currentDropdown
                                |> Expect.equal (Just (toImmediateDropdown Fonts))
                    , test "selects the textbox" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "N")
                                |> .annotation
                                |> Expect.equal TextBox
                    ]
                , describe "and another dropdown is open"
                    [ test "opens the fonts dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown Fills)
                                |> Controls.onKeyDown (key "N")
                                |> .currentDropdown
                                |> Expect.equal (Just (toImmediateDropdown Fonts))
                    , test "selects the textbox" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown Fills)
                                |> Controls.onKeyDown (key "N")
                                |> .annotation
                                |> Expect.equal TextBox
                    ]
                ]
            , describe "when K is pressed"
                [ describe "and the stroke colors dropdown is open"
                    [ test "closes the dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown StrokeColors)
                                |> Controls.onKeyDown (key "K")
                                |> .currentDropdown
                                |> Expect.equal Nothing
                    ]
                , describe "and the stroke colors dropdown is closed"
                    [ test "opens the stroke colors dropdown" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "K")
                                |> .currentDropdown
                                |> Expect.equal (Just (toImmediateDropdown StrokeColors))
                    ]
                , describe "and another dropdown is open"
                    [ test "opens the stroke colors dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown Fills)
                                |> Controls.onKeyDown (key "K")
                                |> .currentDropdown
                                |> Expect.equal (Just (toImmediateDropdown StrokeColors))
                    ]
                ]
            , describe "when F is pressed"
                [ describe "and the fills dropdown is open"
                    [ test "closes the dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown Fills)
                                |> Controls.onKeyDown (key "F")
                                |> .currentDropdown
                                |> Expect.equal Nothing
                    ]
                , describe "and the fills dropdown is closed"
                    [ test "opens the fills dropdown" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "F")
                                |> .currentDropdown
                                |> Expect.equal (Just (toImmediateDropdown Fills))
                    ]
                , describe "and another dropdown is open"
                    [ test "opens the fills dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown StrokeColors)
                                |> Controls.onKeyDown (key "F")
                                |> .currentDropdown
                                |> Expect.equal (Just (toImmediateDropdown Fills))
                    ]
                ]
            , describe "when S is pressed"
                [ describe "and the strokes dropdown is open"
                    [ test "closes the dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown Strokes)
                                |> Controls.onKeyDown (key "S")
                                |> .currentDropdown
                                |> Expect.equal Nothing
                    ]
                , describe "and the strokes dropdown is closed"
                    [ test "opens the strokes dropdown" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "S")
                                |> .currentDropdown
                                |> Expect.equal (Just (toImmediateDropdown Strokes))
                    ]
                , describe "and another dropdown is open"
                    [ test "opens the strokes dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown StrokeColors)
                                |> Controls.onKeyDown (key "S")
                                |> .currentDropdown
                                |> Expect.equal (Just (toImmediateDropdown Strokes))
                    ]
                ]
            ]
        , describe "update"
            [ describe "when selecting a fill"
                [ test "updates the fill" <|
                    \_ ->
                        initialState
                            |> Controls.update (SelectFill fill)
                            |> .annotationStyles
                            |> .fill
                            |> Expect.equal fill
                , test "closes the dropdown" <|
                    \_ ->
                        initialState
                            |> withDropdown (toImmediateDropdown Fills)
                            |> Controls.update (SelectFill fill)
                            |> .currentDropdown
                            |> Expect.equal Nothing
                ]
            , describe "when selecting a stroke color"
                [ test "updates the stroke color" <|
                    \_ ->
                        initialState
                            |> Controls.update (SelectStrokeColor strokeColor)
                            |> .annotationStyles
                            |> .strokeColor
                            |> Expect.equal strokeColor
                , test "closes the dropdown" <|
                    \_ ->
                        initialState
                            |> withDropdown (toImmediateDropdown StrokeColors)
                            |> Controls.update (SelectStrokeColor strokeColor)
                            |> .currentDropdown
                            |> Expect.equal Nothing
                ]
            , describe "when selecting a stroke style"
                [ test "updates the stroke style" <|
                    \_ ->
                        initialState
                            |> Controls.update (SelectStrokeStyle strokeStyle)
                            |> .annotationStyles
                            |> .strokeStyle
                            |> Expect.equal strokeStyle
                , test "closes the dropdown" <|
                    \_ ->
                        initialState
                            |> withDropdown (toImmediateDropdown Strokes)
                            |> Controls.update (SelectStrokeStyle strokeStyle)
                            |> .currentDropdown
                            |> Expect.equal Nothing
                ]
            , describe "when selecting a font size"
                [ test "updates the font size" <|
                    \_ ->
                        initialState
                            |> Controls.update (SelectFontSize fontSize)
                            |> .annotationStyles
                            |> .fontSize
                            |> Expect.equal fontSize
                , test "closes the dropdown" <|
                    \_ ->
                        initialState
                            |> withDropdown (toImmediateDropdown Fonts)
                            |> Controls.update (SelectFontSize fontSize)
                            |> .currentDropdown
                            |> Expect.equal Nothing
                ]
            , describe "when waiting for a dropdown to open"
                [ test "tracks the dropdown with waiting set to true" <|
                    \_ ->
                        initialState
                            |> Controls.update (WaitForDropdownToOpen shapesDropdown)
                            |> .currentDropdown
                            |> Maybe.map isWaiting
                            |> Maybe.withDefault False
                            |> Expect.true "Expected a delayed dropdown with waiting set to true"
                ]
            , describe "when cancelling a delayed dropdown"
                [ test "it stops tracking the dropdown" <|
                    \_ ->
                        { initialState | currentDropdown = Just waitingDropdown }
                            |> Controls.update CancelDropdownWait
                            |> .currentDropdown
                            |> Expect.equal Nothing
                ]
            , describe "when toggling a dropdown"
                [ describe "and the dropdown is open"
                    [ test "it closes the dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown (toImmediateDropdown Fills)
                                |> Controls.update (ToggleDropdown (toImmediateDropdown Fills))
                                |> .currentDropdown
                                |> Expect.equal Nothing
                    ]
                , describe "and the dropdown is closed"
                    [ test "it opens the dropdown" <|
                        \_ ->
                            initialState
                                |> Controls.update (ToggleDropdown (toImmediateDropdown Fills))
                                |> .currentDropdown
                                |> Maybe.map (Expect.equal (toImmediateDropdown Fills))
                                |> Maybe.withDefault (Expect.fail "Expected a dropdown to be open")
                    ]
                ]
            , describe "when changing the selected drawing" <|
                [ test "it closes any dropdowns" <|
                    \_ ->
                        initialState
                            |> withDropdown (toImmediateDropdown Fills)
                            |> Controls.update (ChangeAnnotation FreeHand)
                            |> .currentDropdown
                            |> Expect.equal Nothing
                , test "it tracks the new drawing" <|
                    \_ ->
                        initialState
                            |> Controls.update (ChangeAnnotation FreeHand)
                            |> .annotation
                            |> Expect.equal FreeHand
                ]
            ]
        ]
