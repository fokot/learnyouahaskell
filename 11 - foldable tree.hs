import Data.Foldable
import qualified Data.Foldable as F
import Data.Monoid

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord, Read)


instance Foldable Tree where
	--foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
	foldMap f Empty = mempty
	foldMap f (Node a l r) = foldMap f l `mappend`
						     f a `mappend`
						     foldMap f r

testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )

treeToSum = F.foldl (+) 0 testTree
-- 42

treeToProduct = F.foldl (*) 1 testTree
-- 64800

treeToList = F.foldMap (\x -> [x]) testTree
-- [1,3,6,5,8,9,10]



--class Foldable t where
--
--    -- | Combine the elements of a structure using a monoid.
--    fold :: Monoid m => t m -> m
--    fold = foldMap id

--    -- | Map each element of the structure to a monoid,
--    -- and combine the results.
--    foldMap :: Monoid m => (a -> m) -> t a -> m
--    foldMap f = foldr (mappend . f) mempty
--    .......