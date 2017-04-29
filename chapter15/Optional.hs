module Optional where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend x Nada    = x
  mappend Nada y    = y
  mappend (Only x) (Only y) = Only (mappend x y)
