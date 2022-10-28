module BTree where

import Prelude hiding (sum, max, min)

data Node a = Branch a (Node a) (Node a) | Leaf a

instance (Show a) => Show (Node a) where
  show (Leaf n) = show n
  show (Branch n l r) = "(" ++ show l ++ " " ++ show n ++ " " ++ show r ++ ")"

instance (Eq a) => Eq (Node a) where
  (==) (Leaf a) (Leaf b) = a == b
  (==) (Branch a l r) (Branch b l' r') = a == b && l == l' && r == r'
  (==) _ _ = False

instance Functor Node where
  fmap f (Leaf n) = Leaf (f n)
  fmap f (Branch n l r) = Branch (f n) (fmap f l) (fmap f r)

instance Applicative Node where
  pure = Leaf
  (Leaf f) <*> n = fmap f n
  (Branch f l r) <*> (Leaf n) = Branch (f n) (l <*> Leaf n) (r <*> Leaf n)
  (Branch f l r) <*> (Branch n l' r') = Branch (f n) (l <*> l') (r <*> r')

instance Monad Node where
  return = pure
  (Leaf n) >>= f = f n
  (Branch n _ _) >>= f = f n

value :: Node a -> a
value (Leaf x) = x
value (Branch x _ _) = x

size :: Node a -> Int
size (Leaf _) = 1
size (Branch _ l r) = 1 + size l + size r

sum :: (Num a) => Node a -> a
sum (Leaf x) = x
sum (Branch x l r) = x + sum l + sum r

avg :: (Fractional a) => Node a -> a
avg n = sum n / fromIntegral (size n)

max :: (Num a) => Node a -> a
max (Leaf x) = x
max (Branch _ _ r) = max r

min :: (Num a) => Node a -> a
min (Leaf x) = x
min (Branch _ l _) = min l

find :: (Ord a) => Node a -> a -> Maybe (Node a)
find n@(Leaf nv) v = if nv == v then Just n else Nothing
find n@(Branch nv l r) v
  | nv == v = Just n
  | v < nv = find l v
  | nv < v = find r v