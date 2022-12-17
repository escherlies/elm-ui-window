module Window.Utils exposing (..)

{-| Extra stuff like helpers and utility functions
-}

-- Combinators


{-| Call a function `f` twice with an argument `x`.

Also known as Warbler (`λab.abb`)

-}
callTwice : (a -> a -> b) -> a -> b
callTwice a b =
    a b b
