module Chapter15 where
import Data.Semigroup ( Sum )
import Data.Monoid ( Sum )
import Test.QuickCheck ( Gen, Function(..), CoArbitrary, Arbitrary(..), quickCheck, frequency )

{-
    Exercise: Optional Monoid
-}
data Optional a = Nop | Yep a
    deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nop = Nop
    fmap f (Yep x) = Yep (f x)

instance (Semigroup a) => Semigroup (Optional a) where
    Yep x <> Yep y = Yep $ x <> y
    Nop <> opt = opt
    opt <> Nop = opt

instance (Monoid a) => Monoid (Optional a) where
    mempty = Nop

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = frequency [(1, return Nop), (1, Yep <$> arbitrary)]

{-
    Refactor: Madlibbin
-}
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
            -> Adverb
            -> Noun
            -> Adjective
            -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter' :: Exclamation
                    -> Adverb
                    -> Noun
                    -> Adjective
                    -> String
madlibbinBetter' e adv noun adj =
    mconcat [
        e, "! he  said "
        , adv, " as he jumped into his car "
        , noun, " and drove off with his "
        , adj, " wife."
    ]

{- Exercise: Alternative monoid for Optional a -}
newtype First' a = First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Semigroup (First' a) where
    x@(First' (Yep _)) <> _ = x
    _ <> y = y

instance Monoid (First' a) where
    mempty = First' Nop

instance (Arbitrary a) => Arbitrary (First' a) where
    arbitrary = First' <$> arbitrary

checkFirst' :: IO ()
checkFirst' = do
    quickCheck (isSemigroup :: First' Integer -> First' Integer -> First' Integer -> Bool)
    quickCheck (hasLeftIdentity :: First' Integer -> Bool)
    quickCheck (hasRightIdentity :: First' Integer -> Bool)


{- Semigroup exercises -}

-- 1: Trivial
data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

checkTrivial :: IO ()
checkTrivial = do
    quickCheck (isSemigroup :: Trivial -> Trivial -> Trivial -> Bool)
    quickCheck (hasLeftIdentity :: Trivial -> Bool)
    quickCheck (hasRightIdentity :: Trivial -> Bool)

-- 2: Identity
newtype Identity a = Identity a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

type IdInt = Identity (Sum Integer)
checkIdentity :: IO ()
checkIdentity = do
    quickCheck (isSemigroup :: IdInt -> IdInt -> IdInt -> Bool)
    quickCheck (hasLeftIdentity :: IdInt -> Bool)
    quickCheck (hasRightIdentity :: IdInt -> Bool)

-- 3: Two
data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

type TwoInt = Two (Sum Int) (Sum Int)
checkTwo :: IO ()
checkTwo = do
    quickCheck (isSemigroup :: TwoInt -> TwoInt -> TwoInt -> Bool)
    quickCheck (hasLeftIdentity :: TwoInt -> Bool)
    quickCheck (hasRightIdentity :: TwoInt -> Bool)

-- 4: Three
data Three a b c = Three a b c
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type ThreeInt = Three (Sum Int) (Sum Int) (Sum Int)
checkThree :: IO ()
checkThree = do
    quickCheck (isSemigroup :: ThreeInt -> ThreeInt -> ThreeInt -> Bool)
    quickCheck (hasLeftIdentity :: ThreeInt -> Bool)
    quickCheck (hasRightIdentity :: ThreeInt -> Bool)

-- 6: BoolConj
newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)

instance Monoid BoolConj where
    mempty = BoolConj True

instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> arbitrary

checkBoolConj :: IO ()
checkBoolConj = do
    quickCheck (isSemigroup :: BoolConj -> BoolConj -> BoolConj -> Bool)
    quickCheck (hasLeftIdentity :: BoolConj -> Bool)
    quickCheck (hasRightIdentity :: BoolConj -> Bool)

-- 7: BoolDisj
newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (b1 || b2)

instance Monoid BoolDisj where
    mempty = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> arbitrary

checkBoolDisj :: IO ()
checkBoolDisj = do
    quickCheck (isSemigroup :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
    quickCheck (hasLeftIdentity :: BoolDisj -> Bool)
    quickCheck (hasRightIdentity :: BoolDisj -> Bool)

-- 8: Or
data Or a b = Fst a | Snd b
    deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd x) <> _ = Snd x
    _ <> (Snd y) = Snd y
    _ <> (Fst x) = Fst x

-- The Or type cannot have a monoid instance since there isn't a unit for <>.
--  (Snd z) cannot be a unit since, when used as the first argument, the second argument will be ignored.
--  (Fst x) cannot be a unit since, when used as the second argument, the first argument will be ignored.
-- Since neither value acts as a neutral element, we cannot have a monoid.

-- 9: Combine
--   This is the first non-trivial monoid.
--   It is essentially a monoid based on parallel function application to produce two monoids that are combined.
--
--   TODO: Figure out how to test the Combine monoid. Right now, it doesn't work because there is no Eq instance.
--     How the hell would Eq work?
--     Maybe a different API entry in Test.QuickCheck will allow side-stepping this issue?
--     Date: 13/06/2022
newtype Combine a b = Combine { uncombine :: a -> b }

instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \x -> f x <> g x

instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ const mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = Combine <$> arbitrary

instance (Show a, Show b, Function a) => Show (Combine a b) where
    show x = show . function . uncombine $ x

type CombineInt2Int = Combine Int (Sum Int)
checkCombine :: IO ()
checkCombine = undefined

-- 10: Comp
newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp $ f . g

instance Monoid (Comp a) where
    mempty = Comp id

-- 11: Validation
data Validation a b = Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Success x) <> _ = Success x
    _ <> (Success y) = Success y
    (Failure x) <> (Failure y) = Failure $ x <> y

instance Monoid a => Monoid (Validation a b) where
    mempty = Failure mempty

{- Monoid exercises -}
-- Implement monoids for each type above that has a semigroup

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
    m1 <> m2 = Mem {
        runMem = \s0 ->
            let (a1, s1) = runMem m1 s0
                (a2, s2) = runMem m2 s1
            in (a1 <> a2, s2)
    }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ (,) mempty

{-
    Tests for Chapter 15
      Semigroup operator associativity
      Monoid identity
-}
isAssociative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
isAssociative (<>) x y z =
    x <> (y <> z) == (x <> y) <> z

isSemigroup :: (Eq a, Semigroup a) => a -> a -> a -> Bool
isSemigroup = isAssociative (<>)

hasLeftIdentity :: (Eq a, Monoid a) => a -> Bool
hasLeftIdentity m = mempty <> m == m

hasRightIdentity :: (Eq a, Monoid a) => a -> Bool
hasRightIdentity m = m <> mempty == m

isCommutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
isCommutative (<>) x y =
    x <> y == y <> x
