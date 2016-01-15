-- code from Arrow tutorial https://wiki.haskell.org/Arrow_tutorial
-- for arrow do notation
{-# LANGUAGE Arrows #-}

module ArrowFun where
import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))

newtype SimpleFunc a b = SimpleFunc {
    runF :: (a -> b)
}

instance Arrow SimpleFunc where
    arr f = SimpleFunc f
    first (SimpleFunc f) = SimpleFunc (mapFst f)
                  where mapFst g (a,b) = (g a, b)
    second (SimpleFunc f) = SimpleFunc (mapSnd f)
                  where mapSnd g (a,b) = (a, g b)

instance Category SimpleFunc where
  (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
  id = arr id

-- Arrow composition is achieved with (>>>). This takes two arrows and chains them together, one after another.
-- It is also arrow- specific. Its signature is:
-- (>>>) :: (Arrow a) => a b c -> a c d -> a b d


-- Split is an arrow that splits a single value into a pair of duplicate values:
split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x,x))

-- Unsplit is an arrow that takes a pair of values and combines them to return a single value:
unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry
-- arr (\op (x,y) -> x `op` y)

-- (***) combines two arrows into a new arrow by running the two arrows on a pair of values (one arrow on the first item of the pair and one arrow on the second item of the pair).
f *** g = first f >>> second g

-- (&&&) combines two arrows into a new arrow by running the two arrows on the same value:
f &&& g = split >>> first f >>> second g
-- = split >>> f *** g

-- LiftA2 makes a new arrow that combines the output from two arrows using a binary operation. It works by splitting a value and operating on both halfs and then combining the result:
liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = split >>> first f >>> second g >>> unsplit op
-- = f &&& g >>> unsplit op


---------------- EXAMPLE ----------------
f, g :: SimpleFunc Int Int
f = arr (`div` 2)
g = arr (\x -> x*3 + 1)

h :: SimpleFunc Int Int
h = liftA2 (+) f g

hOutput :: Int
hOutput = runF h 8

-- the same using do
h' :: SimpleFunc Int Int
h' = proc x -> do
      fx <- f -< x
      gx <- g -< x
      returnA -< (fx + gx)

hOutput' :: Int
hOutput' = runF h' 8

-- Kleisi
-- newtype Kleisli m a b = Kleisli {
--   runKleisli :: (a -> m b)
-- }

plusminus, double, h2 :: Kleisli [] Int Int
plusminus = Kleisli (\x -> [x, -x])
double    = arr (* 2)
h2        = liftA2 (+) plusminus double

h2Output :: [Int]
h2Output = runKleisli h2 8

-- Finally, here is a little teaser. There is an arrow function called returnA which returns an identity arrow.
-- There is an ArrowPlus class that includes a zeroArrow (which for the list monad is an arrow that always returns
-- the empty list) and a <+> operator (which takes the results from two arrows and concatenates them).
-- We can build up some pretty interesting string transformations (the multi-valued function String -> [String])
-- using Kleisli arrows:

main :: IO ()
main = do
    let
        prepend x = arr (x ++)
        append  x = arr (++ x)
        withId  t = returnA <+> t
        xform = (withId $ prepend "<") >>>
                (withId $ append ">") >>>
                (withId $ ((prepend "!") >>> (append "!")))
        xs = ["test", "foobar"] >>= (runKleisli xform)
    mapM_ putStrLn xs