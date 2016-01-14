module Main where

import Prelude hiding (and)
import Data.Map
import Control.Exception.Base
import Control.Applicative

data Address = Address {
  number :: Int,
  street :: String
}


--data Result = Pass | Fail {message :: [String]} deriving Show

--type Rule a = a -> Result

--nonEmpty :: Rule String
--nonEmpty "" = Fail ["Empty String"]
--nonEmpty _  = Pass

--gte :: Int -> Rule Int
--gte min value
--  | value < min = Fail ["Too small"]
--  | otherwise   = Pass

--and :: Rule a -> Rule a -> Rule a
--and r1 r2 a = case ((r1 a), (r2 a)) of 
--  (Pass, Pass) -> Pass
--  (Pass, (Fail a)) -> Fail a
--  ((Fail a), Pass) -> Fail a
--  ((Fail a), (Fail b)) -> Fail (a ++ b)

--checkAddress :: Rule Address
--checkAddress = (gte 1 . number) `and` (nonEmpty . street)


data Result a = Pass a
              | Fail {message :: [String]} deriving Show

type Rule a b = a -> Result b


type FormData = Map String String

get :: String -> FormData -> Result String
get _ _ = Pass "a" -- Data.Map.lookup

parseInt :: String -> Result Int
parseInt x = case reads x of
               (n, _) : _ -> Pass n
               [] -> Fail ["Not a number"]

readNumber :: Rule FormData Int
readNumber form = case get "number" form of
                    Pass a -> parseInt a
                    Fail e -> Fail e

--(>>=) :: Result a -> (a -> Result b) -> Result b
--(Fail a) >>= _ = Fail a
--(Pass a) >>= f = f a

instance Functor Result where
  fmap f (Pass a) = Pass (f a)
  fmap f (Fail a) = Fail a

instance Applicative Result where
  pure = Pass
  (Pass f) <*> (Pass a) = Pass (f a)
  (Pass f) <*> (Fail a) = Fail a
  (Fail a) <*> (Fail b) = Fail (a ++ b)

instance Monad Result where
  (Fail a) >>= _ = Fail a
  (Pass a) >>= f = f a
  return = pure

readNumber2 form = get "number" form >>= parseInt

readNumber3 form = do
  number <- get "number" form
  parseInt number


and :: Result a -> Result b -> (a -> b -> c) -> Result c
and (Pass a) (Pass b) f = Pass (f a b)
and (Pass _) (Fail b) _ = Fail b
and (Fail a) (Pass _) _ = Fail a
and (Fail a) (Fail b) _ = Fail (a ++ b)

readAddress :: FormData -> Result Address
readAddress form = let number = readNumber form
                       street = (get "street") form
                   in  and number street Address


readAddress2 form = Address <$> (readNumber form) <*> ((get "street") form)

readAddress3 form = liftA2 Address (readNumber form) ((get "street") form)

