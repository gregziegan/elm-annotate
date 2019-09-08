module TestUtil exposing (getFirstAnnotation, isAnnotationMovedByCorrectAmount, position)

import Annotate exposing (Model)
import Annotation exposing (Annotation)
import Array
import Fuzz exposing (Fuzzer)
import Position exposing (EndPosition, Position, StartPosition)
import Random
import Shrink


getFirstAnnotation : Model -> Maybe Annotation
getFirstAnnotation model =
    model
        |> .edits
        |> .present
        |> Array.get 0


position : Fuzzer Position
position =
    Fuzz.custom
        (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
        (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))


isAnnotationMovedByCorrectAmount : Position -> Position -> ( StartPosition, EndPosition ) -> Annotation -> Bool
isAnnotationMovedByCorrectAmount start end ( origStart, origEnd ) shiftedAnnotation =
    let
        ( shiftedStart, shiftedEnd ) =
            Annotation.startAndEnd shiftedAnnotation

        dx =
            shiftedEnd.x - shiftedStart.x

        dy =
            shiftedEnd.y - shiftedStart.y
    in
    Position.shift dx dy origStart
        == shiftedStart
        && Position.shift dx dy origEnd
        == shiftedEnd
