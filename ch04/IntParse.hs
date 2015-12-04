import Data.Char (digitToInt)

asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
  in loop acc' xs

asInt_fold :: String -> Int
asInt_fold (x:xs) = 
  if x=='-' then
      (foldl step 0 xs) * (-1)
    else
      foldl step 0 (x:xs)
    where step acc x = digitToInt(x) + 10 * acc
