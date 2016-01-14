-- 99 problems from https://wiki.haskell.org/99_questions/

-- 2
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast (x : xs) = if length xs == 1 
  then x
  else myButLast xs

myButLast2 (a : b : []) = a
myButLast2 as = myButLast as

-- 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength [] = 0

-- 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse xs = myReverse' xs []
  where myReverse' [] acc = acc
        myReverse' (x:xs) acc = myReverse' xs (x:acc)

-- without tailrec
myReverse2 :: [a] -> [a]
myReverse2 (x:xs) = myReverse2 xs ++ [x]
myReverse2 [] = []


-- 8
-- Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress (x:y:z) = if x == y then rest else [x] ++ rest
  where rest = compress (x:z)
compress x = x

-- 11
-- Modified run-length encoding.
data Encoded a = Single {value :: a} 
               | Multiple {times :: Int, value :: a}

instance Show a => Show (Encoded a) where
  show (Single a) = show a
  show (Multiple n a) = show (n, a)

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = flip encodeModified' []
  where
    increment (Single a) = Multiple 2 a               
    increment m = Multiple (times m + 1) (value m)
    encodeModified' [] acc = acc
    encodeModified' (x:xs) [] = encodeModified' xs [Single x]
    encodeModified' (x:xs) acc@(a:as) =
      if x == (value a)
        then encodeModified' xs (increment a:as)
        else encodeModified' xs (Single x:acc)

