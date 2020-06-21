f :: (Functor f, Show a) => f a -> f String
f x = fmap show x