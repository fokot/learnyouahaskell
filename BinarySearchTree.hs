module BinarySearchTree where
import qualified Data.Tree as T
import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace

data Tree a = Nil | Node {
  value :: a,
  left :: Tree a,
  right :: Tree a
} deriving (Show)


newNode a = Node a Nil Nil

empty :: Tree a -> Bool
empty Nil = True
empty _   = False

size :: Tree a -> Int
size Nil = 0
size (Node _ l r) = 1 + size l + size r

contains :: (Ord a) => a -> Tree a -> Bool
contains _ Nil = False
contains a (Node v l r)
  | a == v = True
  | a < v  = contains a l
  | True   = contains a r

insert :: Ord a => a -> Tree a -> Tree a
-- insert a t | contains a t = t
insert a Nil = newNode a
insert a t@(Node v l r)
  | a == v = t
  | a <  v = Node v (insert a l) r
  | a >  v = Node v l (insert a r)

insertAll :: Ord a => [a] -> Tree a -> Tree a
insertAll a t = foldl (flip insert) t a

delete :: Ord a => a -> Tree a -> Tree a
delete a Nil = Nil
delete a (Node v l r)
  | a == v = combineSubtrees l r -- combining l and r so root value is deleted
  | a <  v = Node v (delete a l) r
  | a >  v = Node v l (delete a r)
  where combineSubtrees Nil Nil = Nil
        combineSubtrees Nil r = r
        combineSubtrees l Nil = l
        combineSubtrees (Node v ll lr) r = Node v (combineSubtrees ll lr) r

-- result is sorted
toList :: Ord a => Tree a -> [a]
toList Nil = []
toList (Node a l r) = toList l ++ a : toList r

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

data PrintData a = PrintData {
  val :: a,
  leftOffset :: Int
}

toDataTree :: Tree a -> Maybe (T.Tree a)
toDataTree Nil = Nothing
toDataTree (Node v l r) = Just $ T.Node v $ catMaybes [(toDataTree l), (toDataTree r)]


showTree :: Show a => Tree a -> String
showTree t = fromMaybe "" $ fmap T.drawTree (toDataTree (fmap show t))

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . showTree

-- balances the tree
balance :: Ord a => Tree a -> Tree a
balance t = balance' (toList t) where
  balance' :: [a] -> Tree a
  balance' []   = Nil
  balance' list = let (l, a, r) = splitListInMiddle list
                in Node a (balance' l) (balance' r)

splitListInMiddle :: [a] -> ([a], a, [a])
splitListInMiddle list =
  let len = length list
      halfLen = ceiling $ (fromIntegral len) / 2
      (l, r) = splitAt (halfLen - 1) list
  in  (l, head r, tail r)


depth :: Tree a -> Int
depth Nil = 0
depth (Node a l r) = 1 + max (depth l) (depth r)



testTree = insertAll [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18] Nil
testTreePrint = printTree testTree

testTreeBalanaced = balance testTree
testTreeBalanacedPrint = printTree testTreeBalanaced