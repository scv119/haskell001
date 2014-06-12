factorial :: Integer -> Integer

factorial n =  
  let
    loop :: Integer -> Integer -> Integer
    loop i j
      | i > 1 = loop (i-1) (j * i)
      | otherwise = j :: Integer
  in loop n 1

distanceFromOrigin :: Point -> Double
distanceFromOrigin (Pt x y) = sqrt(x^2 + y^2)

data Point = Pt Double Double
    deriving (Show, Eq)

data Color = Sarcroline | Coquelicot | Smaragdine
    deriving (Eq, Show)

hipster :: Color -> String

hipster Sarcroline = "flesh coloured"
hipster _ = "watever"

main = print (hipster Coquelicot)
