import Control.Applicative

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
-- different possible implementation
--sequenceA = foldr (liftA2 (:)) (pure [])  
-- my shitty implementation
--sequenceA as = foldr (\a b -> (:) <$> a <*> b) (pure []) as

-- help for my implementation
--foldr :: (a -> b -> b) -> b -> [a] -> b
--(<$>) :: Functor f => (a -> b) -> f a -> f b
--(<*>) :: Applicative f => f (a -> b) -> f a -> f b