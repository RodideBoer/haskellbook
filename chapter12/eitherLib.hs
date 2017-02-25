module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr leftOnly []
    where leftOnly (Left y) ys = y:ys
          leftOnly _        ys = ys

-- the real lefts is pretty cool:
-- lefts x = [ a | Left a <- x ]

rights' :: [Either a b] -> [b]
rights' = foldr rightOnly []
    where rightOnly (Right y) ys = y:ys
          rightOnly _         ys = ys

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right y) = Just (f y)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
-- without help from the Linter it was:
--eitherMaybe'' f x = either' (\_ -> Nothing) (\y -> Just (f y)) x
