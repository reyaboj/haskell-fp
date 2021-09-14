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