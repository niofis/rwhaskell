splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred (x:xs) 
  | (pred x) == True = x : (splitWith pred xs)
  | otherwise = []
