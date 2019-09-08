module AnnotationTests exposing (suite)

import Annotation
import Annotation.Options exposing (StrokeStyle(..))
import Expect
import Palette
import Test exposing (..)


defaults =
    Annotation.defaultStyles


suite : Test
suite =
    describe "Annotation"
        [ describe "defaultStyles"
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
