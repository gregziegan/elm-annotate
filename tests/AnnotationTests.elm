module AnnotationTests exposing (suite)

import Annotation exposing (Choice(..))
import Annotation.Options exposing (StrokeStyle(..))
import AutoExpand
import Expect
import Fuzz exposing (Fuzzer, int, string)
import Palette
import Position exposing (Position)
import Test exposing (..)
import TestUtil exposing (position)


defaults =
    Annotation.defaultStyles


alteredStyles =
    { defaults | strokeColor = Palette.red }


{-| TODO: create default config function
-}
config : Annotation.Config ()
config =
    Annotation.configure
        { events = []
        , translate = ( 0, 0 )
        , snap = False
        , onInput = \_ -> ()
        , onFocus = ()
        , eventsForVertex = Nothing
        }


start =
    Position 0 0


end =
    Position 10 10


arrow =
    Annotation.init config { id = 1, start = start, end = end, choice = Arrow, styles = defaults }


textBox =
    Annotation.init config { id = 1, start = start, end = end, choice = TextBox, styles = defaults }


someString : Fuzzer String
someString =
    Fuzz.map
        (\s ->
            if String.length s == 0 then
                s ++ "something"

            else
                s
        )
        string


autoExpandConfig =
    AutoExpand.config
        { onInput = \_ -> ()
        , padding = 0
        , minRows = 1
        , maxRows = 4
        , lineHeight = 12
        }


suite : Test
suite =
    describe "Annotation"
        [ describe "hasNoText"
            [ test "returns true when text is empty" <|
                \_ ->
                    Expect.equal True (Annotation.hasNoText { textBox | text = "" })
            , fuzz someString "returns false when text is not empty" <|
                \str ->
                    Expect.equal False (Annotation.hasNoText { textBox | text = str })
            ]
        , describe "startAndEnd"
            [ fuzz2 position position "returns start and end in a tuple" <|
                \pos1 pos2 ->
                    let
                        annotation =
                            { arrow | start = pos1, end = pos2 }
                    in
                    Expect.equal ( annotation.start, annotation.end ) (Annotation.startAndEnd annotation)
            ]
        , describe "setStyles"
            [ test "tracks styles in the .styles field" <|
                \_ ->
                    arrow
                        |> Annotation.setStyles alteredStyles
                        |> .styles
                        |> Expect.equal alteredStyles
            ]
        , describe "updateTextBox"
            [ describe "tracks"
                [ fuzz string "text updates" <|
                    \text ->
                        textBox
                            |> Annotation.updateTextBox (AutoExpand.initState autoExpandConfig) text
                            |> .text
                            |> Expect.equal text
                , test "auto-expand changes" <|
                    \_ ->
                        textBox
                            |> Annotation.updateTextBox (AutoExpand.initState autoExpandConfig) ""
                            |> .autoExpand
                            |> Expect.equal (AutoExpand.initState autoExpandConfig)
                ]
            ]
        , describe "move"
            [ fuzz2 int int "shifts start and end positions by x and y" <|
                \dx dy ->
                    Annotation.move ( dx, dy ) arrow
                        |> Annotation.startAndEnd
                        |> Expect.equal ( Position.shift dx dy arrow.start, Position.shift dx dy arrow.end )
            ]
        , describe "defaultStyles"
            [ test "has a purple stroke color" <|
                \_ ->
                    Expect.equal Palette.purple defaults.strokeColor
            , test "has an empty fill" <|
                \_ ->
                    Expect.equal Nothing defaults.fill
            , test "has a solid stroke style" <|
                \_ ->
                    Expect.equal SolidMedium defaults.strokeStyle
            , test "has a medium font size" <|
                \_ ->
                    Expect.equal 20 defaults.fontSize
            ]
        ]
