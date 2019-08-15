module Annotate.Environment exposing
    ( Environment
    , OperatingSystem(..)
    )

{-| Details that won't change at runtime.


# Annotation Environment

@docs Environment


# Supported Operating Systems

@docs OperatingSystem

-}


{-| -}
type alias Environment =
    { operatingSystem : OperatingSystem
    }


{-| -}
type OperatingSystem
    = MacOS
    | Windows
