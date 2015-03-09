module St where

  data St a b = St (a -> (b, St a b))

  ap :: St a b -> [a] -> [b]
  ap _ [] = []
  ap f (x : xs) = f x : (ap f xs)
  -- don't know how to solve this exercise
