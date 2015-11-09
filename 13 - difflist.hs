import Control.Monad.Writer

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- fast, cos DiffList is fast in prepending
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

-- low, cos list is slow in prepending
finalCountDownSlow :: Int -> Writer [String] ()
finalCountDownSlow 0 = do
    tell ["0"]
finalCountDownSlow x = do
    finalCountDownSlow (x-1)
    tell [show x]

fast = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000

slow = mapM_ putStrLn . snd . runWriter $ finalCountDownSlow 500000
