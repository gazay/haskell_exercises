module LSystem where

import Prelude hiding ((>>))
import Kleisli

generate :: Int -> (a -> [a]) -> (a -> [a])
{-
generate 0 f = idK
generate n f = f *> generate(n - 1) f
-}

generate n f = iterate (*> f) idK !! n

gen :: Int -> String
gen n = generate n next 'a'

next :: Char -> String
next 'a' = "ab"
next 'b' = "a"
