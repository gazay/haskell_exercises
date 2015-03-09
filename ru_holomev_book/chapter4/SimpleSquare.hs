module SimpleSquare where
  import Prelude

  circleSq :: Float -> Float
  circleSq a = pi * a^2

  trnglSq :: Float -> Float -> Float -> Float
  trnglSq a b c = (\p -> sqrt(p * (p - a) * (p - b) * (p - c)))
                  ((a + b + c) / 2)

  rectSq :: Float -> Float -> Float
  rectSq a b = a * b

  trapSq :: Float -> Float -> Float -> Float
  trapSq a b h = ((a + b) / 2) * h
