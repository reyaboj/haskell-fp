module Chapter18 where

import Control.Monad (Monad(..), join, (>=>))
import Control.Applicative (Applicative(..), (*>))
import Test.QuickCheck (Arbitrary(..), frequency)
import Test.QuickCheck.Checkers (EqProp(..), eq, quickBatch)
import Test.QuickCheck.Classes (functor, applicative, monad)

{- Exercise: write bind using fmap and join -}
bind :: (Monad m) => (a -> m b) -> m a -> m b
bind mf = join . fmap mf

{- Desugaring do-notation -}
sequencing :: IO ()
sequencing = do
    -- using do-notation
    putStrLn "A"
    putStrLn "B"

sequencing' :: IO ()
sequencing' =
    -- using explicit (>>)
    putStrLn "A" >>
    putStrLn "B"

sequencing'' :: IO ()
sequencing'' =
    -- using ap-discard-left
    putStrLn "A" *>
    putStrLn "B"

varBinding :: IO ()
varBinding = do
    -- using do-notation
    x <- getLine
    putStrLn x

varBinding' :: IO ()
varBinding' =
    -- explicit (>>=)
    getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    -- do-notation
    putStr "name ples: "
    name <- getLine
    putStrLn $ "halo " ++ name ++ "!"

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStr "name ples: " >>
    getLine >>= \name ->
        putStrLn $ "halo " ++ name ++ "!"

twoBinds :: IO ()
twoBinds = do
    putStr "name ples: "
    name <- getLine
    putStr "age ples: "
    age <- getLine
    putStrLn $ "halo " ++ name ++ "! you are only " ++ age ++ " years old!"

twoBinds' :: IO ()
twoBinds' =
    putStr "name ples: " >>
    getLine >>= \name ->
        putStr "age ples: " >>
        getLine >>= \age ->
            putStrLn $ "halo " ++ name ++ "! you are only " ++ age ++ " years old!" 


{- Example monad: List -}
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []

{- Example monad: Maybe -}
data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int
} deriving (Eq, Show)

nonemptyStr :: String -> Maybe String
nonemptyStr "" = Nothing
nonemptyStr s = Just s

nonnegativeNum :: (Num a, Ord a) => a -> Maybe a
nonnegativeNum x
    | x >= 0 = Just x
    | otherwise = Nothing
    
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing
        else Just c

makeSphericalCow :: String ->
                    Int ->
                    Int ->
                    Maybe Cow
makeSphericalCow name' age' weight' =
    -- explicitly thread the values to point of use
    case nonemptyStr name' of
        Nothing -> Nothing
        Just nammy ->
            case nonnegativeNum age' of
                Nothing -> Nothing
                Just agey ->
                    case nonnegativeNum weight' of
                        Nothing -> Nothing
                        Just weighty ->
                            weightCheck $ Cow {name = nammy, age = agey, weight = weighty}

makeSphericalCow' :: String ->
                     Int ->
                     Int ->
                     Maybe Cow
makeSphericalCow' name' age' weight' = do
    -- do-notation for compact code
    nammy <- nonemptyStr name'
    agey <- nonnegativeNum age'
    weighty <- nonnegativeNum weight'
    weightCheck $ Cow nammy agey weighty

makeSphericalCow'' :: String ->
                      Int ->
                      Int ->
                      Maybe Cow
makeSphericalCow'' name' age' weight' =
    nonemptyStr name' >>= \namey ->
        nonnegativeNum age' >>= \agey ->
            nonnegativeNum weight' >>= \weighty ->
                weightCheck $ Cow namey agey weighty

{- Example of difference between monad and applicative -}
f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
    if even i
        then Just (i+1)
        else Nothing

h :: Integer -> Maybe String
h i = Just $ "10191-" ++ show i

doSomeshizz :: Integer -> Maybe (Integer, Integer, String)
doSomeshizz n = do
    -- using do-notation
    x <- f n  -- nonzero check
    y <- g x  -- check if even, then get next odd num
    z <- h y  -- craft a string
    return (x, y, z)

doSomeshizz' :: Integer -> Maybe (Integer, Integer, String)
doSomeshizz' n =
    -- try to implement the OG using applicative ONLY
    let x = f n
        y = g <$> x
        z = (h <$>) <$> y  -- z has type Maybe (Maybe (Maybe String))
    -- at a loss how to unnest without join
    in undefined


{- Example monad: Either -}
type Founded = Int
type Coders = Int

data SoftwareShop = Shop {
    founded :: Founded,
    programmers :: Coders
} deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

makeSoftware :: Int -> Int -> Either FoundedError SoftwareShop
makeSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders
    if programmers > founded `div` 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers

{- Exercise: Either monad -}
data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure = Second

    (Second f) <*> (Second b) = Second $ f b
    (Second _) <*> (First a) = First a
    (First a) <*> (Second _) = First a
    (First a) <*> (First a') = First a  -- bias towards the earliest error

instance Monad (Sum a) where
    return = pure

    mb >>= f =
        case mb of
            First a -> First a
            Second b -> f b

data CountMe a = CountMe Integer a deriving (Eq,Show)

instance Functor CountMe where
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    (CountMe n f) <*> (CountMe n' a) = CountMe (n+n') (f a)

instance Monad CountMe where
    return = pure
    (CountMe n a) >>= f =
        let (CountMe n' b) = f a
        in CountMe (n+n') b

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
    (=-=) = eq

countMeTests :: IO ()
countMeTests = do
    let trigger :: CountMe (Int, String, Int)
        trigger = undefined
    quickBatch . functor $ trigger
    quickBatch . applicative $ trigger
    quickBatch . monad $ trigger

{- Monad composition -}
mcomp :: (Monad m) =>
            (b -> m c)
        ->  (a -> m b)
        -> a -> m c
mcomp f g a = g a >>= f

sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Belo, how old r u? "


{- Chapter EXERCISES: Monad instances -}

-- 1: Nope monad
data Nope a = NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return = pure
    _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

nopeTests :: IO ()
nopeTests = do
    let trigger :: Nope (Int, Int, Int)
        trigger = undefined
    quickBatch . functor $ trigger
    quickBatch . applicative $ trigger
    quickBatch . monad $ trigger

-- 2: ReverseEither monad (ridiculous name PhhhbbtttEither in the book)
data ReverseEither b a = RLeft a | RRight b
    deriving (Eq, Show)

instance Functor (ReverseEither b) where
    fmap _ (RRight b) = RRight b
    fmap f (RLeft a) = RLeft $ f a

instance Applicative (ReverseEither b) where
    pure x = RLeft x

    (RLeft f) <*> (RLeft x) = RLeft $ f x
    (RRight b) <*> _ = RRight b
    _ <*> (RRight b) = RRight b

instance Monad (ReverseEither b) where
    return = pure

    ma >>= f =
        case ma of
            RRight b -> RRight b
            RLeft a -> f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (ReverseEither b a) where
    arbitrary = frequency [(1, RLeft <$> arbitrary), (1, RRight <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (ReverseEither b a) where
    (=-=) = eq

reverseEitherTests :: IO ()
reverseEitherTests = do
    let trigger :: ReverseEither String (Int, Int, Int)
        trigger = undefined
    quickBatch . functor $ trigger
    quickBatch . applicative $ trigger
    quickBatch . monad $ trigger

-- 3: Identity monad
newtype Identity a = Identity a
    deriving (Eq, Show, Ord)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

identityTests :: IO ()
identityTests = do
    let trigger :: Identity (Int, Int, Int)
        trigger = undefined
    quickBatch . functor $ trigger
    quickBatch . applicative $ trigger
    quickBatch . monad $ trigger

-- 4: List monad
data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Semigroup (List a) where
    xs <> Nil = xs
    Nil <> ys = ys
    (Cons x xs) <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure = flip Cons Nil

    Nil <*> _ = Nil
    (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
    return = pure
    xs >>= f = foldr (<>) Nil . fmap f $ xs

instance Foldable List where
    foldr _ b Nil = b
    foldr f b (Cons x xs) = f x (foldr f b xs)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(1, return Nil), (2, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
    (=-=) = eq

listTests :: IO ()
listTests = do
    let trigger :: List (Int, Int, Int)
        trigger = undefined
    quickBatch . functor $ trigger
    quickBatch . applicative $ trigger
    quickBatch . monad $ trigger


{- Chapter EXERCISES: Implement functions using Monad/Functor methods -}

-- 1
j :: Monad m => m (m a) -> m a
j m = m >>= id

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb =
    f <$> ma >>=
        \g -> g <$> mb

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf =
    ma >>=
        \a -> mf >>=
            \f -> return (f a)

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = do
    let h = f a
        t = meh as f
    x <- h
    xs <- t
    return (x:xs)

-- 6
flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id