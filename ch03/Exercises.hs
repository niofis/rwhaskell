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

data Direction = DLeft | DRight | DStraight
  deriving (Show)


