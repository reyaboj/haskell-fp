module Chapter8 where

{-
    Intermission: Exercise

    Evaluate
        applyTimes 5 (+1) 5

    Evaluation using the pointful equation below:
        applyTimes 5 (+1) 5

        (+1)
            (applyTimes (5-1) (+1) 5)

        (+1)
            ( (+1)
                (applyTimes (4-1) (+1) 5) )

        (+1)
            ((+1)
                ((+1)
                    (applyTimes (3-1) (+1) 5) ))

        (+1)
            ((+1)
                ((+1)
                    ((+1)
                        (applyTimes (2-1) (+1) 5) )))

        (+1)
            ((+1)
                ((+1)
                    ((+1)
                        ((+1)
                            (applyTimes (1-1) (+1) 5) ))))

        (+1)
            ((+1)
                ((+1)
                    ((+1)
                        ((+1)
                            (5) ))))

        ... skipping some applications

        10
-}
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ x = x
applyTimes n f x = f . applyTimes (n-1) f $ x
-- bottom equation equivalent to:
-- f (applyTimes (n-1) f x)


{-
    Recursion
-}

-- 2) Recursive sum from 1 to n
sumUpTo :: (Num a, Ord a) => a -> a
sumUpTo n = goSum n 0
    where goSum x tally
            | x <= 0 = tally
            | otherwise = goSum (x-1) (tally+x)

-- 3) Recursive multiply
mul :: (Num a, Ord a) => a -> a -> a
mul x y
    | x < 0 && y < 0 = goMul (-x) (-y) 0
    | y < 0 = goMul (-x) (-y) 0
    | otherwise = goMul x y 0
    where
        goMul _ 0 z = z
        goMul a b z = goMul a (b-1) (z+a)
