module Chapter7 where

{-
    (1) The following function returns the tens digit of an integral value.
-}
tensDigit :: (Integral a) => a -> a
tensDigit x = d
    where
        xLast = x `div` 10
        d = xLast `mod` 10

-- a) rewrite this using divMod
tensDigit' :: (Integral a) => a -> a
tensDigit' x = d1
    where
        (x', _) = x `divMod` 10
        (_, d1) = x' `divMod` 10

-- b) does the divMod version have the same type as the original?
-- ans: yes, because it's still an a -> a function with constraint (Integral a)

-- c) change it to get the hundreds digit
hunsD x = d2
    where
        (x1, _) = x `divMod` 10
        (x2, _) = x1 `divMod` 10
        d2 = x2 `mod` 10


{-
    (2) Implement the function below with type a -> a -> Bool -> a using
        a) case expression
        b) guards
-}
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

-- a)
foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b =
    case b of
        False -> x
        True -> y

-- b)
foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b
    | b == False = x
    | b == True = y


{-
    (3) Fill in the definition to g
-}
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)


{-
    (4) Read and Show classes and pointfree lead up to (5)

    Explanation:
        roundTrip converts a value x to its String representation, followed by converting it back to its original representation.

        Basically a fancy identity function.
-}
roundTrip :: (Read a, Show a) => a -> a
roundTrip x = read (show x)

roundTripConsole :: IO ()
roundTripConsole = do
    print (roundTrip 42)
    print (id 42)


{-
    (5) Write a point-free version of roundTrip
-}
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show


{-
    (6) Change roundTrip type signature to:

            roundTrip :: (Show a, Read b) => a -> b

        and make the expression below work:

            print (roundTrip 4)

        using the type annotation operator ::, and parentheses for scoping
-}

-- Just to explore, I wrote down the new version and ran roundTrip'' 42
-- only to be greeted in ghci by the following error:
--   *** Exception: Prelude.read: no parse
-- I was expecting it NOT to type check, but it did.
-- I made a few attempts to force GHC to deduce that type var a == type var b, but failed.
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show


exprio6 :: IO ()
exprio6 = print (roundTrip'' 42 :: (Num a, Read a) => a)