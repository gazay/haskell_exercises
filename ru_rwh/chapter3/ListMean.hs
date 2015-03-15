listMean :: [Int] -> Float
listMean [] = 0
listMean xs = fromIntegral(sum xs) / fromIntegral(length xs)
