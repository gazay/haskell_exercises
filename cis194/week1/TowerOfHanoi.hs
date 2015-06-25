module TowerOfHanoi where

type Peg = String
type Move = (Peg, Peg)

-- hanoi 2 "a" "b" "c" => [("a", "b"), ("a", "c"), ("b", "c")]
-- hanoi 3 "a" "b" "c" => [("a", "c"), ("a", "b"), ("c", "b"), ("a", "c"), ("b", "a"), ("b", "c"), ("a", "c")]
-- hanoi 4 "a" "b" "c" => [("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b"), ("a", "c"), ("b", "c"), ("b", "a"), ("c", "a"), ("b", "c"), ("a", "b"), ("a", "c"), ("b", "c")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 f _ l = [(f,l)]
hanoi pegs f tmp l = hanoi (pegs - 1) f l tmp ++ [(f,l)] ++ hanoi (pegs - 1) tmp f l

-- hanoi4 2 "a" "b" "c" "d" => [("a", "b"), ("a", "d"), ("b", "d")]
-- hanoi4 3 "a" "b" "c" "d" => [("a", "b"), ("a", "c"), ("a", "d"), ("c", "d"), ("b", "d")]
-- hanoi4 4 "a" "b" "c" "d" => [("a", "d"), ("a", "b"), ("a", "c"), ("d", "b"), ("a", "d"), ("c", "d"), ("b", "a"), ("b", "d"), ("a", "d")]
-- hanoi4 5 "a" "b" "c" "d" => [("a", "d"), ("a", "b"), ("a", "c"), ("d", "b"), ("a", "d"), ("c", "d"), ("b", "a"), ("b", "d"), ("a", "d")]
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 f _ _ l = [(f,l)]
hanoi4 pegs f tmp1 tmp2 l = undefined
