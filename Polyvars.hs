{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Polyvars where

class SumArgs a where
  sumArgs :: [Int] -> a

instance (SumArgs r) => SumArgs (Int -> r) where
  sumArgs is i = sumArgs (i:is)

instance SumArgs Int where
  sumArgs = sum

sumOf' :: SumArgs args => args
sumOf' = sumArgs []

f = 5 :: Int
g = 4 :: Int

fg = sumOf' f g :: Int
-- sumOf' f g :: Int
-- 9

fgffg = sumOf' f g f f g :: Int
-- sumOf' f g f f g :: Int
-- 23

instance (SumArgs r) => SumArgs ([Int] -> r) where
  sumArgs is n = sumArgs (n ++ is)

fgArray = sumOf' [f, g] :: Int
-- sumOf' [f, g] :: Int
-- 9

fgfgArray = sumOf' f g [f, g] :: Int
-- sumOf' f g [f, g] :: Int
-- 18