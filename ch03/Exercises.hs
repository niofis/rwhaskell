import Data.List

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList (Cons x xs) = x : (toList xs)
toList Nil = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data TreeM a = NodeM a (Maybe (TreeM a)) (Maybe (TreeM a))
              deriving(Show)

myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength [] = 0

reverseL :: [a] -> [a]
reverseL (x:xs) = (reverseL xs) ++ [x]
reverseL [] = []

palindrome :: [a] -> [a]
palindrome x = x ++ (reverseL x)

compareL :: (Eq a) => [a] -> [a] -> Bool
compareL (a:as) (b:bs) = (a == b) && (compareL as bs)
compareL [] [] = True

is_palindrome :: (Eq a) => [a] -> Bool
is_palindrome x
  | (mod (length x) 2) /= 0 = False
  | otherwise = compareL (take half x) (reverseL (drop half x))
    where half = div (length x)  2

compareLength :: [a] -> [a] -> Ordering
compareLength a b = compare (length a) (length b)

orderL :: [[a]] -> [[a]]
orderL x = sortBy compareLength x

intersp :: a -> [[a]] -> [a]
intersp s (x:xs) 
  | null xs = x 
  | otherwise = x ++ [s] ++ (intersp s xs)
intersp _ [] = []

treeHeight :: Tree a -> Int
treeHeight (Node _ left right) = 1 + (max (treeHeight left) (treeHeight right))
treeHeight Empty = 0

meanL :: [Float] -> Float 
meanL x = (sumL x) / (fromIntegral (length x))
  where 
    sumL (y:ys) = y + (sumL ys)
    sumL [] = 0

data Direction = DLeft | DRight | DStraight
  deriving (Show)

data Point = Point Float Float
  deriving (Show)

slope :: Point -> Point -> Float
slope (Point x1 y1) (Point x2 y2) = (y2-y1)/(x2-x1)

getDirection :: Point -> Point -> Point -> Direction
getDirection v1 v2 v3
  | pb > pa = DLeft
  | pb < pa = DRight
  | otherwise = DStraight
  where
    pa = slope v1 v2
    pb = slope v2 v3

getDir :: Point -> Point -> Point -> Direction
getDir (Point x1 y1) (Point x2 y2) (Point x3 y3)
  | cz > 0 = DLeft
  | cz < 0 = DRight
  | otherwise = DStraight
    where
      cz = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

getDirections :: [Point] -> [Direction]
getDirections (x:xs)
  | (length xs) < 2 = []
  | otherwise = (getDir x (head xs) (head (tail xs))) : (getDirections xs)

leftSort :: [Point] -> [Point]
leftSort xs = sortBy compareP xs
  where
    compareP (Point x1 y1) (Point x2 y2)
      | c == EQ = compare x1 x2
      | otherwise = c
        where
          c = compare y1 y2


slopeSort :: [Point] -> [Point]
slopeSort xs = sortBy ss xs
  where
    ss (Point x1 y1) (Point x2 y2) = compare (y1/x1) (y2/x2) 

convexHull :: [Point] -> [Point]
convexHull xs = 
  let lm = leftSort xs
      leftmost = head lm
      sortedxs = slopeSort (tail lm)
    in
      walkHull [leftmost]  sortedxs
      where 
        walkHull hull (y:ys)
          | (null ys) = y : hull
          | otherwise =
            let dir = getDir (head hull) y (head ys) in
              if dir == DRight then
                walkHull ((head ys) : hull) (drop 2 ys)
              else
                walkHull ([y] ++ [(head ys)] ++ hull) (drop 2 ys) 
            
            
