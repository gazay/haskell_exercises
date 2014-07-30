module Stream where
  import qualified Prelude as P
  import Prelude (Show(..), Num(..), Int(..), Bool(..), (<=), (<), (-), error, (+))

  data Stream a = a :& Stream a

  instance Show a => Show (Stream a) where
    show xs = showInfinity (show (take 5 xs))
      where showInfinity x = P.init x P.++ "..."

  instance Num a => Num (Stream a) where
    (+) (a :& as) (b :& bs) = a + b :& as + bs
    (-) (a :& as) (b :& bs) = a - b :& as - bs
    (*) (a :& as) (b :& bs) = a * b :& as * bs

    abs _ = error "abs is undefined for Stream"
    signum _ = error "signum is undefined for Stream"

    fromInteger _ = error "fromInteger is undefined for Stream"

    negate _ = error "negate is undefined for Nat"

  infixl 5 :&

  head :: Stream a -> a
  head (x :& xs) = x

  tail :: Stream a -> Stream a
  tail (x :& xs) = xs

  take :: Int -> Stream a -> [a]
  take n _ | n <= 0 = []
  take n (x :& xs) = x : take (n - 1) xs

  (!!) :: Stream a -> Int -> a
  xs     !! n | n < 0 = error "Index can not be negative"
  (x :& _)  !! 0         = x
  (_ :& xs) !! n         = xs !! (n - 1)

  map :: (a -> b) -> Stream a -> Stream b
  map f (x :& xs) = f x :& map f xs

  filter :: (a -> Bool) -> Stream a -> Stream a
  filter f (x :& xs) = if (f x) then x :& filter f xs else filter f xs

  zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
  zipWith f (x :& xs) (y :& ys) = f x y :& zipWith f xs ys

  zip :: Stream a -> Stream b -> Stream (a, b)
  zip (x :& xs) (y :& ys) = (x, y) :& zip xs ys

  iterate :: (a -> a) -> a -> Stream a
  iterate f a = a :& iterate f (f a)

  booms :: Int -> Stream Int
  booms a = a :& booms (a + 1)
