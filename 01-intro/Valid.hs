toDigits :: Integer -> [Integer]

toDigits number = 
  let 
    toDigits_ :: Integer -> [Integer] -> [Integer]
    toDigits_ a b 
      | (a <= 0) = b
      | otherwise = toDigits_ (quot a 10) ((mod a 10) : b)
  in toDigits_ number []

rev :: [Integer] -> [Integer]
rev arr =
  let 
    rev_ :: [Integer] -> [Integer] -> [Integer]
    rev_ a b = case a of
      y:ys -> rev_ ys (y : b)
      _ -> b
  in rev_ arr []

toDigitsRev :: Integer -> [Integer]

toDigitsRev number = 
  let 
    ret = toDigits number
  in rev ret 

len :: [Integer] -> Integer
len x = case x of
  y:ys -> 1 + (len ys)
  _ -> 0


doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther arr =
  let 
    rarr = rev arr
    double_ :: [Integer] -> [Integer] -> [Integer]
    double_ a b = case a of
      x : y : z -> double_ z ((y*2) : x : b)
      x : z -> double_ z (x : b)
      _ -> b
  in double_ rarr []


sumDigits :: [Integer] -> Integer

sumDigits list =
  let 
    dsum :: Integer -> Integer
    dsum x 
      | (x <= 9) = x
      | otherwise = x - 9
    sum_ :: [Integer] -> Integer -> Integer
    sum_ arr ret = case arr of 
      x : y -> sum_ y (ret + (dsum x))
      _ -> ret
  in sum_ list 0

validate :: Integer -> Bool

validate number = 
  let 
    arr = toDigits number
    arr1 = doubleEveryOther arr
    s = sumDigits arr1
  in ((mod s 10) == 0)

main = print (validate 4012888888881882)
