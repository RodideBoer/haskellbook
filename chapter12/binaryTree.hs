module BinaryTree where

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
    Nothing      -> Leaf
    Just (a,b,c) -> Node (unfold f a) b (unfold f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfold (\y -> if y == x then Nothing else Just (y+1, y, y+1)) 0
