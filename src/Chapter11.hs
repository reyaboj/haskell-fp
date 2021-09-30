{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Chapter11 where
import qualified Data.Char as Char

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
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord xs'@(x:xs)
    | Char.isLower x = Char.toUpper x : xs
    | otherwise = xs'

-- Write a function that capitalizes the first word of each sentence in a string
capitalizeParagraph :: String -> String
capitalizeParagraph = undefined