data Move = Rock | Paper | Scissor
  deriving (Eq, Show)

data Outcome = Lose | Tie | Win
  deriving (Show, Eq, Ord)

outcome :: Move -> Move -> Outcome

outcome m1 m2 = 
  case (m1, m2) of
    (Rock, Scissor) -> Win
    (Scissor, Paper) -> Win
    (Paper, Rock) -> Win
    _ | m1 == m2 -> Tie 
      | otherwise -> Lose


main = print(outcome Rock Paper)
