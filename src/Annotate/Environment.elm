module Annotate.Environment exposing (Environment, OperatingSystem(..))


type alias Environment =
    { operatingSystem : OperatingSystem
    }


type OperatingSystem
    = MacOS
    | Windows
