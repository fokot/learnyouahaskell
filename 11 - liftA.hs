-- inspired by function in PureScript
-- https://leanpub.com/purescript/read#leanpub-auto-applicative-validation

module Main where

import Control.Applicative

-- class Applicative
--pure :: a -> f a
--Lift a value.

--(<*>) :: f (a -> b) -> f a -> f b
--Sequential application.

--(*>) :: f a -> f b -> f b
--Sequence actions, discarding the value of the first argument.

--(<*) :: f a -> f b -> f a
--Sequence actions, discarding the value of the second argument.


(<$$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$$> a = fmap f a

liftA' :: (Applicative f) => (a -> b) -> f a -> f b
liftA' = fmap

liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$$> a <*> b

liftA3' :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3' f a b c = f <$$> a <*> b <*> c