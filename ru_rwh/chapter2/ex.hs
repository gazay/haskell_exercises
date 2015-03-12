lastButOne :: [a] -> a
lastButOne [] = error "List is empty"
lastButOne [x] = error "List does not have enough elements"
lastButOne xs = reverse xs !! 1
