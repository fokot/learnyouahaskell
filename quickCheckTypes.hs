module Main where

import Data.Char

class Generator a where
  x :: a -> a
  x = id

instance (Generator a, Generator b) => Generator (a -> b) where

instance Generator Int where

instance Generator Char where

instance Generator a => Generator [a] where


test :: Generator a => a -> Int
test a = 0

f1 :: Int -> Int -> Int
f1 a b = a + b

_ = test f1

f2 :: Int -> Char -> Int
f2 a b = a + (ord b)

_ = test f2

f3 :: Int -> String -> Int
f3 = undefined

_ = test f3

f4 :: Int -> Int
f4 = undefined

_ = test f4

f5 :: Bool -> Bool
f5 = undefined

-- does not compile
--_ = test f5

