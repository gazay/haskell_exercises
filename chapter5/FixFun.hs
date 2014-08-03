module FixFun where
  import Prelude
  import Data.Function

  repeat' :: a -> [a]
  -- repeat' a = a : repeat' a
  -- repeat' a = fix $ \xs -> a : xs
  repeat' = fix . (:)

  cycle' :: [a] -> [a]
  -- cycle' x = x ++ cycle' x
  -- cycle' x = fix $ \xs -> x ++ xs
  cycle' = fix . (++)

  map' :: (a -> b) -> [a] -> [b]
  {-
  map' f [] = []
  map' f (x:xs) = f x : map' f xs

  1.
  y [] = []
  y (x:xs) = f x : y xs

  2.
  y = \arr -> case arr of
                [] -> []
                (x:xs) -> f x : y xs

  3.
  y = (\t -> \arr -> case arr of
                       [] -> []
                       (x:xs) -> f x : t xs) y

  4.
  y = g y
      where g = \t arr -> case arr of
                            [] -> []
                            (x:xs) -> f x : t xs

  5.
  -}
  map' f = fix g
           where g t = \arr -> case arr of
                                 [] -> []
                                 (x:xs) -> f x : t xs

  foldr' :: (a -> b -> b) -> b -> [a] -> b
  {-
  foldr' f acc [] = acc
  foldr' f acc (x:xs) = f x (foldr' f acc xs)

  y [] = acc
  y (x:xs) = f x (y xs)

  y = \arr -> case arr of
                [] -> acc
                (x:xs) -> f x (y xs)

  y = (\t -> \arr -> case arr of
                       [] -> acc
                       (x:xs) -> f x (t xs)) y
  -}
  foldr' f acc = fix g
                 where g t = \arr -> case arr of
                                       [] -> acc
                                       (x:xs) -> f x (t xs)

  foldl' :: (b -> a -> b) -> b -> [a] -> b
  {-
  foldl' f acc [] = acc
  foldl' f acc (x:xs) = foldl' f (f acc x) xs

  y acc [] = acc
  y acc (x:xs) = y (f acc x) xs

  y acc = \arr case arr of
                 [] = acc
                 (x:xs) = y (f acc x) xs

  y acc = (\t arr -> case arr of
                       [] -> acc
                       (x:xs) -> t (f acc x) xs) y
  foldl' f = \acc arr -> case arr of
                           [] -> acc
                           (x:xs) -> foldl' f (f acc x) xs
  -}
  foldl' f = fix g
             where g t = \acc arr -> case arr of
                                       [] -> acc
                                       (x:xs) -> t (f acc x) xs

  zip' :: [a] -> [b] -> [(a,b)]
  {-
  zip' (a:as) (b:bs) = (a,b) : zip' as bs
  zip' _ _ = []

  zip' = \x y -> case (x,y) of
                   ((a:as), (b:bs)) -> (a,b) : zip' as bs
                   otherwise -> []
  -}
  zip' = fix g
         where g t = \x y -> case (x, y) of
                               ((a:as), (b:bs)) -> (a, b) : zip' as bs
                               (_, _) -> []

  iterate' :: (a -> a) -> a -> [a]
  {-
  iterate' f x = x : iterate f (f x)
  -}
  iterate' f = fix g
               where g t = \x -> x : t (f x)
