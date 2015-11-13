import Data.Monoid

-- Sum is already taken
newtype Summ n = Summ {number :: n} deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Summ a) where
	mempty = Summ 0
	Summ a `mappend` Summ b = Summ (a + b)


testSumm = number . mconcat $ map Summ [1..10]