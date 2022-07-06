{-# LANGUAGE DeriveFunctor #-}
module Chapter20 where

import Data.Foldable ()
import Data.Monoid (Sum (..), Product (..), Any (..))

{- Foldable Examples -}

-- Identity
newtype Identity a = Identity a
    deriving (Eq, Show, Functor)

instance Foldable Identity where
    foldMap f (Identity a) = f a
    
    foldr f z (Identity a) = f a z

    foldl f z (Identity a) = f z a

-- Maybe (Call it Optional to prevent clash)
data Optional a = Empty | Boxed a
    deriving (Eq, Show, Functor)

instance Foldable Optional where
    foldMap _ Empty = mempty
    foldMap f (Boxed a) = f a

    foldr f z Empty = z
    foldr f z (Boxed a) = f a z
    
    foldl f z Empty = z
    foldl f z (Boxed a) = f z a

-- Add my own: Tree
data Tree a = Leaf | Node a [Tree a]
    deriving (Eq, Show, Functor)

treeOfLife :: Tree Int
treeOfLife =
    Node 5
        [
            Node 1
                [Node 2 [], Node (-2) []]
            ,
            Leaf
            ,
            Node 69
                [Node 1 [], Node 2 []]
        ]

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Node a trees) = mconcat $ f a : fmap (foldMap f) trees


{- Exercise: Library functions using Foldable (foldMap | foldr) -}

-- 1: sum
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2: product
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3: elem
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (==a))

-- 4: minimum
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' ta
    | null ta   = Nothing
    | otherwise = Just $ foldr1 min ta

-- 5: maximum
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' ta
    | null ta   = Nothing
    | otherwise = Just $ foldr1 max ta

-- 6: null
null' :: (Foldable t) => t a -> Bool
null' = foldr (const . const False) True

-- 7: length
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const $ Sum 1)

-- 8: toList
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9: fold
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10: foldMap
foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr ((<>) . f) mempty


{- Chapter EXERCISES: Write Foldable instances -}

-- 1: Constant
newtype Constant a b = Constant b deriving (Eq, Show, Functor)

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

-- 2: Two
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

-- 3: Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

-- 4: Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
    foldMap f (Three' a b b') = f b <> f b'

-- 5: Four'
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
    foldMap f (Four' a b b' b'') = f b <> f b' <> f b''


-- Write a filter for Foldable types using foldMap
filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
            (a -> Bool) -> t a -> f a
filterF p = foldMap (\a -> if p a then pure a else mempty)
