import Data.Monoid

newtype Lastt a = Lastt {getLast :: Maybe a} deriving Show

instance Monoid (Lastt a) where
	mempty = Lastt Nothing
	x `mappend` Lastt Nothing = x
	_ `mappend` Lastt x = Lastt x


testLastt = getLast . mconcat . map Lastt $ [Nothing, Just 9, Just 10]  
-- Just 10 