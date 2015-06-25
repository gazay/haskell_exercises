module ValidCard where

-- 1234
-- 1234 % 10 => 4
--
-- [3 1 2 4]
-- 123 => [3 1 2]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0     = []
  | otherwise  = snd result : toDigitsRev (fst result)
      where result = divMod n 10

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0     = []
  | otherwise = reverse $ toDigitsRev n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : (y * 2) : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = first + rest
  where first = sum $ toDigits x
        rest  = sumDigits xs

validate :: Integer -> Bool
validate n = rem summed 10 == 0
  where digits  = toDigitsRev n
        doubled = doubleEveryOther digits
        summed  = sumDigits doubled

validate2 :: Integer -> Bool
validate2 n = rem res 10 == 0
    where res = sumDigits . doubleEveryOther . toDigitsRev n
