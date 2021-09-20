# Chapter 10

## Understanding Folds
1. The following code
   ```haskell
   foldr (*) 1 [1..5]
   ```
   will return the same result as which of the following:
   1. `flip (*) 1 [1..5]`
   2. `foldl (flip (*)) 1 [1..5]` :heavy_check_mark:
   3. `foldl (*) 1 [1..5]` :heavy_check_mark:
2. Write out the evaluation steps for the first line in the fragment below:
   ```haskell
   foldl (flip (*)) 1 [1..3]

   let f = flip (*)
   
   e = foldl (flip (*)) 1 [1..3]
     = foldl f 1 [1..3]
     = foldl f (f 1 1) [2,3]
     = foldl f (f (f 1 1) 2) [3]
     = foldl f (f (f (f 1 1) 2) 3) []
     = (f (f (f 1 1) 2) 3)
     = (f (f 1 2) 3)
     = (f (2*1) 3)
     = 3*2
     = 6
   ```
3. One difference between `foldr` and `foldl` is:
   1. `foldr`, not `foldl`, traverses the spine of a list from right to left
   2. `foldr`, not `foldl`, always forces the rest of the fold
   3. `foldr`, not `foldl`, associates to the right :heavy_check_mark:
   4. `foldr`, not `foldl`, is recursive
4. Folds are catamorphisms, which means they are generally used to:
   1. reduce structure
   2. expand structure
   3. render you catatonic
   4. generate infinite data structures
5. The following are simple folds, similar to what you have already seen, but each has at least one error. Fix them.
   1. `foldr (++) ["woot", "WOOT", "woot"]` \
      **Answer:** `foldr (++) [] ["woot", "WOOT", "woot"]`
   2. `foldr max [] "fear is the little death"` \
      **Answer:** `foldr max 'a' "fear is the little death"`
   4. Can this ever return two distinct answers? `foldr (||) True [False, True]` \
      **Answer:** No. It will always return `True` as the fold of an empty list is always `True`.
   5. `foldl ((++) . show) "" [1..5]` \
      **Answer:** `foldl (++) "" (map show[1..5])`
   1. `foldr const 'a' [1..5]` \
      **Answer:** `foldr (flip const) 'a' [1..5]`
   2. `foldr const 0 "tacos"` \
      **Answer:** `foldr (flip const) 0 "tacos"`
   3. `foldl (flip const) 0 "burritos"` \
      **Answer:** `foldl const 0 "burritos"`
   4.  `foldl (flip const) 'z' [1..5]` \
      **Answer:** `foldl const 'z' [1..5]`

## Database Processing
Refer to module `Chapter10.hs`
