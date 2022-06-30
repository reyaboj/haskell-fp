{-# LANGUAGE RankNTypes, FlexibleInstances #-}
module Chapter16 where

import Test.QuickCheck as Qc
import GHC.Arr
import GHC.IO.Handle.Types (HandleType)


-- DATA
data Pair a b = Pair a b
    deriving (Eq, Show)

data Or a b = First a | Second b
    deriving (Eq, Show)

newtype Wrap f a = Wrap (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

-- Rank N type example (natural transformation)
type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- Flipping functor
data Tuple a b = Tuple a b
    deriving (Eq, Show)

instance Functor (Tuple a) where
    fmap f (Tuple a b) = Tuple a (f b)

newtype Flip f a b = Flip (f b a)
    deriving (Eq, Show)

instance Functor (Flip Tuple a) where
    fmap f (Flip (Tuple b a)) =
        Flip (Tuple (f b) a)

unFlipTuple :: Flip Tuple a b -> Tuple b a
unFlipTuple (Flip (Tuple b a)) = Tuple b a

-- INSTANCES
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x (f y)

instance Functor (Or a) where
    fmap = undefined

-- TESTS
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x =
    fmap id x == id x

functorCompose :: (Functor f, Eq (f c)) => f a -> (a -> b) -> (b -> c) -> Bool
functorCompose x f g =
    (fmap g . fmap f $ x) == fmap (g . f) x

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' f g x =
    let f' = applyFun f
        g' = applyFun g
    in  (fmap g' . fmap f' $ x) == fmap (g' . f') x

c16Tests :: IO ()
c16Tests = do
    quickCheck (functorIdentity :: Pair Int Int -> Bool)
    -- error in the line below: no Show instance for (Int -> Char)
    -- quickCheck (functorCompose :: Pair Int Int -> (Int -> Char) -> (Char -> Int) -> Bool)
    quickCheck (functorCompose' :: Fun Int Char -> Fun Char Int -> Pair Int Int -> Bool)


{- Chapter EXERCISES -}

{-
Determine if a valid functor can be written for the types below
-}

-- 1: No. Bool has kind *.
data Bool' = False' | True'

-- 2: Yes. It has kind * -> *
data BoolAndSomethingElse a = False'' a | True'' a

-- 3: Yes. It has kind * -> *
data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- 4: Yes. It has kind * -> *
newtype Mu f = InF { outF :: f (Mu f) }

-- 5: No. D is a type constant.
data D = D (Array Word Word) Int Int


{-
Rearrange arguments to the type constructor so that the Functor instance works.
-}

-- 1: Change (Sum a b) to (Sum b a)
data Sum b a = First' a | Second' b
    deriving (Show)

instance Functor (Sum e) where
    fmap f (First' a) = First' (f a)
    fmap f (Second' b) = Second' b

-- 2: (Company a b c) becomes (Company a c b)
data Company a c b =
    DeepBlue a c
    | Something b
    deriving (Show)

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- 3: (More a b) becomes (More b a)
data More b a =
    L a b a
    | R b a b
    deriving (Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'


{- Write functor instances -}

-- 1
data Quant a b = Finance | Desk a | Bloor b
    deriving (Show)

instance Functor (Quant a) where
    fmap f (Bloor b) = Bloor (f b)
    fmap _ (Desk a) = Desk a
    fmap _ Finance = Finance

-- 2
newtype K a b = K a

instance Functor (K a) where
    fmap _ (K a) = K a

-- 3 Functor for the Flip type above
instance Functor (Flip K a) where
    fmap f (Flip (K b)) = Flip (K (f b))

makeFlipK :: a -> Flip K b a
makeFlipK x = Flip (K x)

unFlipK :: Flip K a b -> b
unFlipK (Flip (K b)) = b

-- 4
newtype EvilGoatyConst a b = GoatyConst b
    deriving Show

instance Functor (EvilGoatyConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
newtype LiftItOut f a = LiftItOut (f a)
    deriving Show

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- 6
data Parappa f g a = DaWrappa (f a) (g a)
    deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa x y) = DaWrappa (f <$> x) (f <$> y)

-- 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving Show

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething x y) = IgnoringSomething x (f <$> y)

-- 8
data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving Show

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious x y z) = Notorious x y (f <$> z)

-- 9
data List a = Nil | Cons a (List a)
    deriving Show

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (f <$> as)

-- 10
data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats
        (GoatLord a)
        (GoatLord a)
        (GoatLord a)
    deriving Show

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats x y z) = MoreGoats (f <$> x) (f <$> y) (f <$> z)

-- 11
data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) = Read (f . g)