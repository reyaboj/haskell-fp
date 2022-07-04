module Chapter17 where

import Control.Applicative (Applicative (..), ZipList(..), liftA3)
import Data.List (elemIndex, intercalate)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Char (isAlphaNum, isDigit)


f x =
    lookup x [
        (3, "helo"),
        (4, "belo"),
        (5, "kfine")
    ]

g x =
    lookup x [
        (7, "beloppoo"),
        (8, "Hai"),
        (9, "okfine")
    ]

h x = lookup x [ (2,3), (5,6), (7,8) ]

m x = lookup x [ (4,10), (8, 13), (1, 9001) ]

{- Mid exercises -}

-- 1
added :: Maybe Integer
added =
    (+3) <$> lookup 3 (zip [1,2,3] [4,5,6])

-- 2
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3
x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max x' y'

-- 4
xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = (+) <$> x'' <*> y''


{- Identity Functor -}
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)

{- Constant functor -}
newtype Constant a b = Constant a
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant a1) <*> (Constant a2) = Constant (a1 <> a2)


{- Validation using Maybe -}
validateLength :: Int -> String -> Maybe String
validateLength n s =
    if length s > n
        then Nothing
        else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person =
    Person Name Address
    deriving (Eq, Show)

makeName :: String -> Maybe Name
makeName s = Name <$> validateLength 5 s

makeAddress :: String -> Maybe Address
makeAddress s = Address <$> validateLength 10 s

makePerson ::
    String
    -> String
    -> Maybe Person
makePerson name address =
    let n = makeName name
        a = makeAddress address
    in Person <$> n <*> a

{- Custom Maybe -}
data Optional a = Empty | Boxed a
    deriving (Eq, Show)

instance Functor Optional where
    fmap _ Empty = Empty
    fmap f (Boxed a) = Boxed (f a)

instance Applicative Optional where
    pure = Boxed
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Boxed f) <*> (Boxed a) = Boxed (f a)

validateLength' :: Integer -> String -> Optional String
validateLength' n s
    | length s > fromInteger n = Empty
    | otherwise = Boxed s

makeName' :: String -> Optional Name
makeName' s = Name <$> validateLength' 5 s

makeAddress' :: String -> Optional Address
makeAddress' s = Address <$> validateLength' 10 s

makePerson' :: String
            -> String
            -> Optional Person
makePerson' name address =
    Person <$> makeName' name <*> makeAddress' address

{- ZipList monoid -}
instance Semigroup a => Semigroup (ZipList a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
    mempty = pure mempty

zipTests :: IO ()
zipTests = quickBatch (monoid $ ZipList [1 :: Sum Int])

{- List Applicative -}
data List a = Nil | Cons a (List a)
    deriving (Eq)

appendList :: List a -> List a -> List a
appendList Nil ys = ys
appendList (Cons x xs) ys = Cons x (appendList xs ys)

foldList :: (a -> b -> b) -> b -> List a -> b
foldList _ b Nil = b
foldList f b (Cons a as) = f a (foldList f b as)

concatList :: List (List a) -> List a
concatList = foldList appendList Nil

flatMapList :: (a -> List b) -> List a -> List b
flatMapList f xs = concatList $ f <$> xs

takeList :: Int -> List a -> List a
takeList 0 _ = Nil
takeList _ Nil = Nil
takeList n (Cons x xs) = Cons x $ takeList (n-1) xs

repeatList :: a -> List a
repeatList x = Cons x $ repeatList x

instance (Show a) => Show (List a) where
    show xs =
        let leftDelim = "["
            rightDelim = "]"
            commaSepValues = intercalate ", " $ strVals xs
        in  leftDelim ++ commaSepValues ++ rightDelim
        where
            strVals Nil = []
            strVals (Cons a as) = show a : strVals as

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure x = Cons x Nil
    fs <*> xs = flatMapList (<$> xs) fs

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [
        (1, return Nil),
        (1, Cons <$> arbitrary <*> arbitrary) ]

listTests :: IO ()
listTests = do
    quickBatch (functor $ Cons (1 :: Int, "foo", 2 :: Int) Nil)
    quickBatch (applicative $ Cons (1 :: Int, "foo", 2 :: Int) Nil)


{- ZipList applicative -}
newtype ZipList' a = ZipList' (List a)
    deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ f <$> xs

instance Applicative ZipList' where
    pure x = ZipList' (repeatList x)
    
    (ZipList' as) <*> (ZipList' bs) = ZipList' $ pairs as bs
        where
            pairs Nil _ = Nil
            pairs _ Nil = Nil
            pairs (Cons f fs) (Cons x xs) = Cons (f x) $ pairs fs xs

instance Semigroup a => Semigroup (ZipList' a) where
    xs <> ys = (<>) <$> xs <*> ys

instance Monoid a => Monoid (ZipList' a) where
    mempty = pure mempty

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' =
                let (ZipList' l) = xs
                in takeList 3000 l
            ys' =
                let (ZipList' r) = ys
                in takeList 3000 r

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = frequency [
        (1, return $ ZipList' Nil),
        (1, ZipList' <$> arbitrary) ]

zipTests' :: IO ()
zipTests' = do
    quickBatch . monoid $ (undefined :: ZipList' (Sum Int))
    quickBatch . functor $ (undefined :: ZipList' (Int, Int, Int))
    quickBatch . applicative $ (undefined :: ZipList' (Int, Int, Int))

{- Validation -}
data Validation e a = Failed e | Succeeded a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failed e) = Failed e
    fmap f (Succeeded x) = Succeeded (f x)

instance Monoid e => Applicative (Validation e) where
    pure = Succeeded

    (Failed e1) <*> (Failed e2) = Failed $ e1 <> e2
    (Failed e1) <*> _ = Failed e1
    _ <*> (Failed e1) = Failed e1
    (Succeeded f) <*> (Succeeded a) = Succeeded $ f a

data ErrCode = Oi | Lmao | Roflmao
    deriving (Show, Eq)

checkName :: String -> Validation [ErrCode] String
checkName s
    | all isAlphaNum s = Succeeded s
    | otherwise = Failed [Oi]

checkAge :: String -> Validation [ErrCode] String
checkAge s
    | all isDigit s = Succeeded s
    | otherwise = Failed [Lmao, Roflmao]

{- Chapter EXERCISES -}

-- 1
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure x = Pair x x
    (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

pairTests :: IO ()
pairTests = do
    quickBatch . functor $ (undefined :: Pair (Int, Int, Int))
    quickBatch . applicative $ (undefined :: Pair (Int, Int, Int))


-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x

    (Two a f) <*> (Two a' a'') = Two (a <> a') (f a'')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

twoTests :: IO ()
twoTests = do
    quickBatch . functor $ (undefined :: Two (Sum Int) (Int, Int, Int))
    quickBatch . applicative $ (undefined :: Two (Sum Int) (Int, Int, Int))


-- 3
data Three a b c = Three a b c deriving (Eq, Show)

threeAp :: (Monoid a, Monoid b) => Three a b (c -> d) -> Three a b c -> Three a b d
threeAp = liftA2 ($)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty

    liftA2 f (Three a1 b1 c1) (Three a2 b2 c2) =
        Three (a1 <> a2) (b1 <> b2) (f c1 c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

threeTests :: IO ()
threeTests = do
    quickBatch . functor $ (undefined :: Three (Sum Int) (Sum Int) (Int, Int, Int))
    quickBatch . applicative $ (undefined :: Three (Sum Int) (Sum Int) (Int, Int, Int))


-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x

    (Three' a f f') <*> (Three' a' b b') = Three' (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

three'Tests :: IO ()
three'Tests = do
    quickBatch . functor $ (undefined :: Three' (Sum Int) (Int, Int, Int))
    quickBatch . applicative $ (undefined :: Three' (Sum Int) (Int, Int, Int))

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty

    (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

fourTests :: IO ()
fourTests = do
    quickBatch . functor $ (undefined :: Four (Sum Int) (Sum Int) (Sum Int) (Int, Int, Int))
    quickBatch . applicative $ (undefined :: Four (Sum Int) (Sum Int) (Sum Int) (Int, Int, Int))

-- 6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
    pure = Four' mempty mempty mempty

    (Four' a a' a'' f) <*> (Four' b b' b'' c) = Four' (a <> b) (a' <> b') (a'' <> b'') (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

four'Tests :: IO ()
four'Tests = do
    quickBatch . functor $ (undefined :: Four' (Sum Int) (Int, Int, Int))
    quickBatch . applicative $ (undefined :: Four' (Sum Int) (Int, Int, Int))


-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 (,,)