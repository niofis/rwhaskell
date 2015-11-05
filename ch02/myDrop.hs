myDrop n xs = 
  if n <= 0 || null xs
    then xs
    else myDrop (n - 1) (tail xs)

lastButOne xs =
  if (length xs) == 2
    then head xs
    else lastButOne (tail xs)
