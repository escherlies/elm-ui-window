module Window.Extra exposing (..)

{-| Extra stuff like helpers and utility functions
-}

-- Combinators


{-| Warbler

    \a b -> a b b

-}
w : (a -> a -> b) -> a -> b
w a b =
    a b b
