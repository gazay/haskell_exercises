import Data.Foldable (foldl', foldr')

myLength :: [a] -> Int
myLength = foldr (\_ acc -> acc + 1) 0

myLengthl :: [a] -> Int
myLengthl = foldl (\acc _ -> acc + 1) 0

myLengthlOpt :: [a] -> Int
myLengthlOpt = foldl' (\acc _ -> acc + 1) 0

myLengthOpt :: [a] -> Int
myLengthOpt = foldr' (\_ acc -> acc + 1) 0

myLength' :: [a] -> Int
myLength' [] = 0
myLength' [x] = 1
myLength' (_:xs) = 1 + myLength xs
