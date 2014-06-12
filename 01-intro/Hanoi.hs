type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi number p1 p2 p3 
  | (number == 1) = [(p1, p2)]
  | otherwise = (hanoi (number - 1) p1 p3 p2) ++ ((p1, p2) : (hanoi (number - 1) p3 p2 p1))

main = print (hanoi 2 "a" "b" "c")
