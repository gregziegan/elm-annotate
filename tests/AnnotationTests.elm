module AnnotationTests exposing (suite)

import Annotation exposing (Choice(..))
import Annotation.Options exposing (StrokeStyle(..))
import Expect
import Fuzz exposing (Fuzzer, string)
import Palette
import Position exposing (Position)
import Test exposing (..)


defaults =
    Annotation.defaultStyles


{-| TODO: create default config function
-}
config : Annotation.Config ()
config =
    { events = []
    , translate = ( 0, 0 )
    , snap = False
    , onInput = \_ -> ()
    , onFocus = ()
    , eventsForVertex = Nothing
    }


textBox =
    Annotation.init config { id = 1, start = Position 0 0, end = Position 10 10, choice = Arrow, styles = defaults }


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
