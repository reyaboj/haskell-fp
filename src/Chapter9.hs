module Chapter9 where

import qualified Data.Char as Char

{-
    Write your own enumFromTo definitions for the types below.
-}
eftBool :: Bool -> Bool -> [Bool]
eftBool start end = go s e []
    where
        s = fromEnum start
        e = fromEnum end
        go s e xs
            | s > e = xs
            | otherwise = go s (e-1) (toEnum e : xs)


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start end = go s e []
    where
        s = fromEnum start
        e = fromEnum end
        go s e xs
            | s > e = xs
            | otherwise = go s (e-1) (toEnum e : xs)


eftInt :: Int -> Int -> [Int]
eftInt start end = go s e []
    where
        s = fromEnum start
        e = fromEnum end
        go s e xs
            | s > e = xs
            | otherwise = go s (e-1) (toEnum e : xs)


eftChar :: Char -> Char -> [Char]
eftChar start end = go s e []
    where
        s = fromEnum start
        e = fromEnum end
        go s e xs
            | s > e = xs
            | otherwise = go s (e-1) (toEnum e : xs)


{-
    Using takeWhile and dropWhile, define a function that takes in a string
    and returns a list of strings, by taking words in the input separated by spaces

    example:
        myWords "i love monke"
        => ["i", "love", "monke"]

    The solution below can be easily parameterized by noting that Char.isSpace predicate can be replaced with any suitable delimited test, and hence via an additional argument to myWords, we can separate any sequence of words given a delimiter predicate.
-}
myWords :: String -> [String]
myWords cs = case firstWord of
                [] -> []
                word -> firstWord : restWords
    where
        cs' = dropWhile Char.isSpace cs
        firstWord = takeWhile (not . Char.isSpace) cs'
        restWords = myWords $ dropWhile (not . Char.isSpace) cs'


{-
    Data.Char exercises
-}

-- 2) Write a function to filter uppercase letters from a string
upperOnly :: String -> String
upperOnly = filter Char.isUpper

-- 3) Write a function to capitalize the first letter of a string
capitalizeFirstLetter :: String -> String
capitalizeFirstLetter cs = prefix ++ upcaseSuffix
    where
        prefix = takeWhile (not . Char.isAlpha) cs
        suffix = dropWhile (not . Char.isAlpha) cs
        upcaseSuffix = case suffix of
            [] -> []
            (x:xs) -> Char.toUpper x : xs

-- 4) Write a new version of the previous function to capitalize the first word
--    assumption: words are separated by blanks such as '\n', ' ', etc.
capitalizeFirstWord :: String -> String
capitalizeFirstWord cs = prefix ++ wordUpcase ++ remaining
    where
        prefix = takeWhile (not . Char.isAlpha) cs
        suffix = dropWhile (not . Char.isAlpha) cs
        wordUpcase = map Char.toUpper $ takeWhile (not . Char.isSpace) suffix
        remaining = dropWhile (not . Char.isSpace) suffix


-- 5) Write a function to capitalize the first letter of a string and return that
-- 6) Write it in point-free form
firstLetterUpcase :: String -> String 
firstLetterUpcase cs =
    case dropWhile (not . Char.isAlpha) cs of
        [] -> []
        xs -> (:[]) . Char.toUpper . head $ xs


{-
    Ciphers.
    SEE module Cipher.hs
-}

{-
    Writing your own standard functions.
-}

-- 1) myOr
myOr :: [Bool] -> Bool
myOr [] = True
myOr (b:bs) = b || myOr bs


-- 2) myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny p = myOr . map p


-- 3) myElem
myElem :: (Eq a) => a -> [a] -> Bool
myElem x = myAny (==x)


-- 4) myReverse
myReverse :: [a] -> [a]
myReverse xs = goReverse xs []
    where
        goReverse [] zs = zs
        goReverse (x:xs) zs = goReverse xs (x:zs)


-- 5) squish
squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss


-- 6) squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f


-- 7) squishAgain; implement using squishMap
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- 8) myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list; no maximum"
myMaximumBy compfn xs = goMaximumBy (head xs) (tail xs)
    where
        goMaximumBy z [] = z
        goMaximumBy z (x:xs) = case z `compfn` x of
            LT -> goMaximumBy x xs
            _ -> goMaximumBy z xs


-- 9) myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list; no maximum"
myMinimumBy compfn xs = goMinimumBy (head xs) (tail xs)
    where
        goMinimumBy z [] = z
        goMinimumBy z (x:xs) = case z `compfn` x of
            GT -> goMinimumBy x xs
            _ -> goMinimumBy z xs


-- 10) myMinimum, myMaximum
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
