module Chapter10 where

{-
    Exercises: Database processing
-}
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
        DbDate (UTCTime (fromGregorian 1911 5 1)
                        (secondsToDiffTime 34123))
    ,   DbNumber 9001
    ,   DbNumber 9001
    ,   DbString "Hello, world!"
    ,   DbDate (UTCTime (fromGregorian 1921 5 1)
                        (secondsToDiffTime 34123))
    ]

-- 1) Write a function that filters for DbDate values and returns a list of UTCTime     values inside
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = map (\(DbDate t) -> t) itemsFiltered
    where
        itemsFiltered = filter isDbDate items

        isDbDate d@(DbDate _) = True
        isDbDate _ = False


-- 2) Write a function that filters for DbNumber values and returns a list of Integer values inside
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = map (\(DbNumber n) -> n) itemsFiltered
    where
        itemsFiltered = filter isDbNumber items

        isDbNumber d@(DbNumber _) = True
        isDbNumber _ = False


-- 3) Write a function to get the most recent date
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate


-- 4) Write a function to sum all DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber


-- 5) Write a function to average all DbNumber values
avgDb :: [DatabaseItem] -> Double 
avgDb items = sumItems / lengthItems
    where
        items' = filterDbNumber items
        sumItems = fromIntegral . sum $ items'
        lengthItems = fromIntegral . length $ items'

{-
    Scans exercises
-}

-- 1) Modify fibs below to return the first 20 fibonacci numbers
fibs :: (Num a) => [a]
fibs = 1 : scanl (+) 1 fibs

fibs20 :: (Num a) => [a]
fibs20 = take 20 fibs

-- 2) Modify fibs to return the fibonacci numbers < 100
fibsLessThan :: (Num a, Ord a) => a -> [a]
fibsLessThan n = takeWhile (< n) fibs

-- 3) Write the factorial function as a scan
facts :: (Integral a) => [a]
facts = scanl (*) 1 [1,2..]

{- Warm-up chapter exercises -}
stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

-- 1a) Generate all 3 tuple combinations (stop,vowel,stop)
svs :: [(Char, Char, Char)]
svs = [(s1,v,s2) | s1 <- stops, v <- vowels, s2 <- stops]

-- 1b) Generate only combinations that begin with a p
svsWithPrefix :: Char -> [(Char, Char, Char)]
svsWithPrefix c = filter ((== c) . fst3) svs
    where fst3 (x, _, _) = x

-- 1c) Make noun verb noun combinations
-- Meh this is boring I will do it someday maybe...

-- 2) What does the function below do
-- average word length
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))

-- 3) Precise (2)
avgWordLength x =
    (/) (sum (map (fromIntegral . length) (words x)))
        (fromIntegral . length $ words x)


{-
    Rewriting functions using folds
-}

-- 1) myOr
myOr :: [Bool] -> Bool 
myOr = foldr (||) False

-- 2) myAny
myAny :: (a -> Bool) -> [a] -> Bool 
myAny pred = myOr . map pred

-- 3) myElem
-- two versions: using foldr/foldl, and using any
myElemFold :: (Eq a) => a -> [a] -> Bool 
myElemFold x = foldr elemtest False
    where 
        elemtest y b = if b then b else y==x

myElemAny :: (Eq a) => a -> [a] -> Bool 
myElemAny x = myAny (==x)

-- 4) Implement reversal
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5) Implement map as a fold
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6) Implement filter as a fold
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr sift []
    where
        sift x xs
            | pred x = x:xs
            | otherwise = xs

-- 7) squish: flatten a list of lists
squishFold :: [[a]] -> [a]
squishFold = foldr (++) []

-- 8) squishMap: map over elements, concat results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squishFold . myMap f

-- 9) squishAgain: flatten list of lists, reuse squishMap
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10) myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list has no maximum"
myMaximumBy cmp (x:xs) = foldr tourney x xs
    where tourney x y = case cmp x y of
                            GT -> x
                            _ -> y

-- 11) myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list has no maximum"
myMinimumBy cmp (x:xs) = foldr tourney x xs
    where tourney x y = case cmp x y of
                            LT -> x
                            _ -> y