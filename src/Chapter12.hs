module Chapter12 where
import Data.Maybe ( fromMaybe )
import Data.Char
import Data.List

{-
    String processing
-}
-- Write a recursive function that takes a string, breaks it into words and replaces "the" with "a"
-- notThe is a helper function
notThe :: String -> Maybe String
notThe s
    | s == "the" = Nothing
    | otherwise = Just s

replaceTheRec :: String -> String
replaceTheRec [] = []
replaceTheRec str
    | null restReplaced = leadReplaced
    | otherwise = leadReplaced ++ restReplaced
    where
        -- recursively replace in the remaining part
        restReplaced = replaceTheRec rest
        -- replace "the" with "a", leaving everything else as is
        leadReplaced = fromMaybe "a" token
        -- detect "the"
        token = notThe leadWord
        -- break the input into two parts
        (leadWord, rest) = break isSpace . dropWhile isSpace $ str

replaceThe :: String -> String
replaceThe s = unwords . map (fromMaybe "a") $ tokens
    where
        tokens = map notThe . words $ s

-- Write a recursive function that takes a string, breaks it into words and counts the number of instances of "the" followed by a word that starts with a vowel

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel str = countHere + countRest
    where
        countRest = countTheBeforeVowel rest1
        countHere
            | w1 == "the" = case w2 of
                [] -> 0
                c:_ ->
                    if c `elem` "aeiou"
                        then 1
                        else 0
            | otherwise = 0

        (w1, rest1) = getTok str
        (w2, rest2) = getTok rest1

        getTok = break isSpace . dropWhile isSpace

countVowels :: String -> Integer
countVowels = sum . map (const 1) . filter (`elem` "aeiou")

-- skipping "validate the word"

{-
    It's only Natural
-}
data Nat = Zero | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Nat
integerToNat z
    | z < 0 =
        error $ "integerToNat: no natural number corresponds to " ++ show z
    | z == 0 = Zero
    | otherwise = Succ (integerToNat (z-1))

{-
    Small library for Maybe
-}

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f ma = case ma of
    Just b' -> f b'
    Nothing -> b

fromMayybee :: a -> Maybe a -> a
fromMayybee a = mayybee a id

list2Maybe :: [a] -> Maybe a
list2Maybe [] = Nothing
list2Maybe (x:_) = Just x

maybe2List :: Maybe a -> [a]
maybe2List Nothing = []
maybe2List (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (ma:mas) = case ma of
    Nothing -> catMaybes mas
    Just x -> x : catMaybes mas


-- This one is a bit tricky, but is easy to understand with the right
-- interpretation. A list of Maybe values represents an ordered sequence
-- of potentially failing computations (i.e., Just | Nothing).
-- We want to convert this sequence to a single Maybe value.
--
-- For the explanation below, keep this interpretation in mind:
--   Maybe a = computation that may fail to produce a, or succeed
--   [Maybe a] = a sequence of failable computations
--
-- Describing the desired behavior recursively:
--   (base) An empty list of Maybes = overall success
--          Why? Because if it had a failure, the list would contain a Nothing.
--   (rec) For a non-empty list of Maybes, we have either
--          1) the first value is a failure; the whole computation fails
--          2) the first value is a success, then we have one of
--               a) the remaining values have at least one failure; so fail
--               b) no failures in the remaining; so we get a list of outcomes
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []  -- empty list of Maybe vals produce empty result
flipMaybe (ma:mas) =
    case ma of
        Nothing -> Nothing  -- A single failure collapses everything
        Just x -> case flipMaybe mas of  -- recursively evaluate the Maybes
            Nothing -> Nothing  -- at least one failure in mas
            Just xs -> Just (x:xs)  -- no failures


{-
    Small library for Either
-}

lefts' :: [Either a b] -> [a]
lefts' = foldr leftCons []
    where
        leftCons e xs =
            case e of
                Left x -> x:xs
                _ -> xs

rights' :: [Either a b] -> [b]
rights' = foldr rightCons []
    where
        rightCons e xs =
            case e of
                Right x -> x:xs
                _ -> xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (l, r)
    where
        l = lefts' es
        r = rights' es

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e =
    case e of
        Left _ -> Nothing
        Right x -> Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g e =
    case e of
        Left x -> f x
        Right y -> g y

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

{-
    Anamorphisms
-}
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr u x =
    case u x of
        Nothing -> []
        Just (y, x') -> y : myUnfoldr u x'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\y -> Just (y, f y))

{-
    Tree unfolding
-}
data BinaryTree a = Leaf
                | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)

unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree u x =
    case u x of
        Nothing -> Leaf
        Just (x', y, x'') ->
            let left = unfoldTree u x'
                right = unfoldTree u x''
            in  Node left y right

treeBuilder :: Integer -> BinaryTree Integer
treeBuilder n =
    unfoldTree
        (\x ->
            if x >= n
                then Nothing
                else Just (x+1, x, x+1))
        0