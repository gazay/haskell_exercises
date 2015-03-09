module Composition where
  import Prelude

  square :: Floating a => a -> a -> a -> a

  {-
  square a b c = sqrt(p a b c * (p a b c - a) * (p a b c - b) * (p a b c - c))
  p a b c = (a + b + c) / 2
  -}

  {-
  square a b c = let p = (a + b + c) / 2 in
                     sqrt ( p * (p - a) * (p - b) * (p - c))
  -}

  square a b c = (\p -> sqrt ( p * (p - a) * (p - b) * (p - c)))
                 ((a + b + c) / 2)

  ifilter :: (a -> Bool) -> [a] -> [a]

  {-
  ifilter p [] = []
  ifilter p (x:xs) = if (p x)
                       then x : ifilter p xs
                       else ifilter p xs
  -}

  {-
  ifilter p [] = []
  ifilter p (x:xs) = case (p x) of
                       True      -> x : ifilter p xs
                       otherwise -> ifilter p xs
  -}

  {-
  -- It's not composition form - just for lulz
  ifilter p [] = []
  ifilter p (x:xs)
          | p x       = x : rest
          | otherwise = rest
          where rest = ifilter p xs
  -}

  ifilter p [] = []
  ifilter p (x:xs) = let rest = ifilter p xs in
                     (\b -> if b then (x:rest) else rest)
                     (p x)

  ieven :: Integral a => a -> Bool

  {-
  ieven a = (rem a 2) == 0
  -}

  {-
  ieven a = let r = rem a 2 in
            r == 0
  -}

  ieven a = (\x -> rem a 2 == x)
            0

  {-
  beside :: Int -> (Int, Int)

  {-
  beside x@(y) = (y, x + 1)
  -}
  -- WTF with this form - I don't know =(
  beside x@(succ y) = (y, succ x)
  -}

  all' :: (a -> Bool) -> [a] -> Bool
  all' p [] = True
  --all' p (x:xs) = p x && all' p xs
  --all' p (x:xs) = (\b -> b && all' p xs) (p x)
  all' p (x:xs) = case p x of
                    True -> all' p xs
                    otherwise -> False
