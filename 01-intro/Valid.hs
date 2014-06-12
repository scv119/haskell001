toDigits :: Integer -> [Integer]

toDigits number = 
  let 
    toDigits_ :: Integer -> [Integer] -> [Integer]
    toDigits_ a b 
      | (a <= 0) = b
      | otherwise = toDigits_ (quot a 10) ((mod a 10) : b)
  in toDigits_ number []

toDigitsRev :: Integer -> [Integer]

toDigitsRev number = 
  let ret = toDigits number
  in reverse ret

len :: [Integer] -> Integer
len x = case x of
  y:ys -> 1 + (len ys)
  _ -> 0

main = print (len (toDigitsRev (107)))
