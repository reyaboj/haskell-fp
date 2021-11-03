{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Chapter11 where
import qualified Data.Char as Char
import qualified Data.List as List

data Price = Price Integer
    deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
    deriving (Eq, Show)

data Airline = PapuAir
            |  CatapultsR'Us
            |  TakeYourChancesUnited
            deriving (Eq, Show)

type PlaneSize = Integer

data Vehicle = Car Manufacturer Price
            |  Plane Airline PlaneSize

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: PlaneSize -> Vehicle
doge = Plane PapuAir

{- Exercises 11.6 -}

-- (2) Define the functions below.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> Bool
areCars = all isCar

-- (3) Write a function to get manufacturer from Vehicle.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- (4) What happens when you use getManu on plane data?
-- Answer: *** Exception: ~/Code/haskell-fp/src/Chapter11.hs:38:1-21: Non-exhaustive patterns in function getManu

-- (5) Add a field for plane size under the Plane constructor and modify the code to compile.

newtype Goats = Goats Int
    deriving (Eq, Show, TooMany, Num)

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany = (>42)

{- Exercies 11.9 -}

-- (1) Write an instance of TooMany for (Int, String)
instance TooMany (Int, String) where
    tooMany (x, _) = x > 42

-- (2) Write another instance for (Int, Int)
instance TooMany (Int, Int) where
    tooMany (x, y) = (x+y) > 42

-- (3) Make another instance for (Num a, TooMany a) => (a, a)
instance (Num a, TooMany a) => TooMany (a,a) where
    tooMany (x, y) = tooMany x && tooMany y


type Name = String
type Age = Integer
data Person =
    Person { name :: Name
            ,age  :: Age }
    deriving (Eq, Show)

data Fiction = Fiction  -- 1
    deriving (Show)

data Nonfiction = Nonfiction  -- 1
    deriving (Show)

data BookType = -- 1+1 = 2
    FictionBook Fiction -- 1
    | NonfictionBook Nonfiction -- 1
    deriving (Show)

type AuthorName = String

-- Just as in real numbers algebra: a*(b+c) = a*b + a*c
-- We can analogously factor types in algebraic datatypes using sum of product
-- It's in normal form when there is no more reduction to be done
data Author1 = Author AuthorName BookType
    deriving (Show)

-- We have a product type: AuthorName * BookType
-- so we can obtain sum-of-products: AuthorName*T1 + AuthorName*T2 + ...
-- AuthorName distributes over all constructors in BookType (FictionBook, NonfictionBook)
data Author2 =
    FictionAuthor AuthorName
    | NonfictionAuthor AuthorName

{- Exercises 11.12 -}
data FlowerType =
    Gardenia
    | Daisy
    | Rose
    | Lilac
    deriving (Show)

type Gardener = String

data Garden = Garden Gardener FlowerType
    deriving (Show)

-- (1) What is the sum-of-products form of Garden type?
data Gardensop =
    GardeniaGarden Gardener
    | DaisyGarden Gardener
    | RoseGarden Gardener
    | LilacGarden Gardener


data GuessWhat =
    ChickenButt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b =
    First a
    | Second b deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct {
        pfirst :: a
    ,   psecond :: b
    } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)


{- Exercises 11.13 -}
-- Write a function that generates all inhabitants of Programmer

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNeverMindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show, Enum)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show, Enum)

data Programmer =
    Programmer {
        os :: OperatingSystem
    ,   lang :: ProgLang
    } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = enumFrom GnuPlusLinux

allProgLangs :: [ProgLang]
allProgLangs = enumFrom Haskell

allProgrammers :: [Programmer]
allProgrammers =
    [Programmer s l | s <- allOperatingSystems, l <- allProgLangs]


{- Exercises 11.17 -}
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x t@(Node left y right)
    | x == y = t
    | x < y = Node (insert' x left) y right
    | x > y = Node left y (insert' x right)

-- Write a map function for the BinaryTree type
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node left' (f x) right'
    where
        left' = mapTree f left
        right' = mapTree f right

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 3 Leaf)
         1
         (Node Leaf 4 Leaf)

expectedTree :: BinaryTree Integer
expectedTree =
    Node (Node Leaf 4 Leaf)
         2
         (Node Leaf 5 Leaf)

mapOkay :: [Char]
mapOkay =
    if mapTree (+1) testTree == expectedTree
        then "okay"
        else "fail"

-- Write tree traversals to convert to list: preorder, inorder, postorder
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ [x] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = postorder left ++ postorder right ++ [x]

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 1 Leaf)
         2
         (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree' == [2,1,3]
        then putStrLn "preorder: passed!"
        else putStrLn "preorder: something went wrong :/"

testInorder :: IO ()
testInorder =
    if inorder testTree' == [1,2,3]
        then putStrLn "inorder: passed!"
        else putStrLn "inorder: something went wrong :/"

testPostorder :: IO ()
testPostorder =
    if postorder testTree' == [1,3,2]
        then putStrLn "postorder: passed!"
        else putStrLn "postorder: something went wrong :/"

-- Write a fold for the tree type
testTree3 :: BinaryTree Integer
testTree3 =
    Node
        (Node
            (Node Leaf 4 Leaf)
            2
            (Node Leaf 6 Leaf))
        1
        (Node
            (Node Leaf 5 Leaf)
            3
            (Node Leaf 7 Leaf))


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left x right) =
    let rightz = foldTree f (f x z) right
    in  foldTree f rightz left

{- Chapter exercises -}

{- As-patterns -}

-- Implement a function to test if A is a subsequence of B
-- Idea: since the elements of A must appear in the same order, we can characterize the solution by stating that:
-- an empty sequence is a subsequence of any sequence
-- a subsequence (a:as) against (b:bs) must match one of the following conditions:
--   a==b AND as is a subsequence of bs
--   a/=b AND (a:as) is a subsequence of bs
isSubSeqOf :: (Eq a) => [a] -> [a] -> Bool
isSubSeqOf [] _ = True
isSubSeqOf _ [] = False
isSubSeqOf xs'@(x:xs) (y:ys)
    | x==y = isSubSeqOf xs ys
    | otherwise = isSubSeqOf xs' ys

-- Split a sentence into words and pair each word with its capitalized variant
-- Example: "I like cake" becomes [("I", "I"), ("like", "Like"), ("cake", "Cake")]
capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = zip xs' (map wordUp xs')
    where
        wordUp [] = []
        wordUp (c:cs) = Char.toUpper c : cs
        xs' = words xs

-- Write a function that capitalizes a word
-- Does not handle untrimmed strings; e.g., " foo"
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord xs'@(x:xs)
    | Char.isLower x = Char.toUpper x : xs
    | otherwise = xs'

-- Write a function that capitalizes the first word of each sentence in a string
capitalizeParagraph :: String -> String
capitalizeParagraph = goCap True
    where
        -- recursive traversal
        -- i swear this could be a fold of some kind but I can't see it right now...
        goCap _ [] = []
        goCap isSentenceHead xs'@(x:xs)
            -- if the next word is the start of a sentence
            | isSentenceHead && Char.isLower x = goCap False $ capitalizeWord xs'

            -- if we see a punctuation mark
            -- caveat: assumes semicolons, colons etc. are also sentence-delimiters, but too lazy to fix this ;)
            | Char.isPunctuation x = x : goCap True xs

            -- anything else just keep traversing
            | otherwise = x : goCap isSentenceHead xs


{-
    Phone exercise

---------------------------
| 1      | 2 ABC | 3 DEF  |
| 4 GHI  | 5 JKL | 6 MNO  |
| 7 PQRS | 8 TUV | 9 WXYZ |
| * ^    | 0 + _ | # . ,  |
---------------------------

Simulate the keypad for old-school phones.

Each digit button, when pressed, cycles through the possibilities as follows.

Keypress | Outcome
------------------
2        | A
22       | B
222      | C
2222     | 2
22222    | A
...      | ...

The key (*) capitalizes the letter that you type next.
-}

-- convert the conversation below to keypresses
convo :: [String]
convo = [
    "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think Im pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
    ]

-- DaPhone represents a keypad of digits with associated characters. There is
-- a key that, when prefixed, produces the uppercase associated character.
-- The structure is not ideal since you could instead associate a character with its
-- corresponding key in a more efficient lookup structure than a list.
data DaPhone =
    DaPhone {
        keypad :: [Key],
        upcaseKey :: Digit
    }

-- A key represents a digit with its associated characters.
-- For example, '6' might be associated with ['m', 'n', 'o'].
type Key = (Digit, [Char])

-- valid buttons: 1234567890*#
type Digit = Char

-- valid presses >= 1
type Presses = Int

-- A simple phone structure
flipPhone :: DaPhone
flipPhone =
    DaPhone {
        upcaseKey = '*',
        keypad = [
            ('1', [])
            , ('2', ['a','b','c'])
            , ('3', ['d','e','f'])
            , ('4', ['g','h','i'])
            , ('5', ['j','k','l'])
            , ('6', ['m','n','o'])
            , ('7', ['p','q','r','s'])
            , ('8', ['t','u','v'])
            , ('9', ['w','x','y','z'])
            , ('*', ['^'])
            , ('0', [' ', '+', '_'])
            , ('#', ['.', ','])
        ]
    }

-- Given a phone and a character to type, produces the key that must be pressed.
-- TODO:  refactor the code by collapsing both branches to a single branch since
--   both branches differ only in their filter predicate
findKey :: DaPhone -> Char -> Key
findKey phone c
    | Char.isNumber c =
        -- search the keys for matching digit and return first match
        case filter ((c==) . fst) $ keypad phone of
            [] -> error $ "findKey: failed to find key for <" ++ [c] ++ ">"
            k:_ -> k

    | otherwise =
        -- search the keys for matching character and return first match
        case filter ((c `elem`) . snd) $ keypad phone of
            [] -> error $ "findKey: failed to find key for <" ++ [c] ++ ">"
            k:_ -> k

-- Example:
--   'a' -> [('2', 1)]
--   'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c = presses
    where
        c' = Char.toLower c

        key@(d, cs) = findKey phone c'

        pos = case List.elemIndex c' cs of
            Nothing -> error $ "reverseTaps: findKey found wrong key " ++ show key
            Just i -> i

        upcasePrefix = (upcaseKey phone, 1)
        charTaps = (d, pos+1)

        presses
            | Char.isNumber c = [(c, length cs + 1)]
            | Char.isUpper c = [upcasePrefix, charTaps]
            | otherwise = [charTaps]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

-- how many times do digits have to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- what was the most popular letter for each message? what was its cost?
mostPopularLetter :: DaPhone -> String -> (Char, Presses)
mostPopularLetter phone message = tapStats
    where
        messageSorted = List.sort message
        messageGrouped =
            List.groupBy (\c1 c2 -> Char.toLower c1 == Char.toLower c2)
                            messageSorted
        messageCounted = map (\cs -> (cs, length cs)) messageGrouped
        tapStats =
            case List.sortBy
                    (\(_, n1) (_, n2) -> n2 `compare` n1)
                    messageCounted of
                        [] -> error "mostPopularLetter: empty list of letter counts"
                        (cs,_):_ -> (Char.toLower . head $ cs,
                                            fingerTaps . cellPhonesDead phone $ cs)

-- what was the most popular letter overall?
coolestLtr :: [String] -> Char
coolestLtr phone = undefined -- meh boring

-- what was the most popular word?
coolestWord :: [String] -> String
coolestWord = undefined      -- meh boring
