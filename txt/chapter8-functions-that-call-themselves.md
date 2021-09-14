# Chapter 8: Functions that Call Themselves

## Review of types
1. What is the type of `[[True, False], [True, True], [False, True]]`?
   1. `Bool`
   2. Mostly `True`
   3. `[a]`
   4. `[[Bool]]` :heavy_check_mark:
2. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`?
   1. `[(True, False), (True, True), (False, True)]`
   2. `[[3==3], [6>5], [3<4]]` :heavy_check_mark:
   3. `[3==3, 6>5, 3<4]`
   4. `["Bool", "more Bool", "Booly Bool!"]`
3. For the following function
   ```haskell
   func :: [a] -> [a] -> [a]
   func x y = x ++ y
   ```
   which of the following is true?
   1. `x` and `y` must be the same type
   2. `x` and `y` must both be lists
   3. if `x` is a `String` then `y` must be a `String`
   4. all of the above :heavy_check_mark:
4. For the `func` code above, which is a valid application to both of its arguments?
   1. `func "Hello World"`
   2. `func "Hello" "World"` :heavy_check_mark:
   3. `func [1,2,3] "a, b, c"`
   4. `func ["Hello", "World"]`

## Review of currying
Given the following definitions, state the value that results from further applications.

```haskell
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types below

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
```

1. What is the value of `appedCatty "woohoo!"`? \
   **Answer:** `"woops mrow woohoo!"`
2. `frappe "1"` \
   **Answer:** `"1 mrow haha"`
3. `frappe (appedCatty "2")` \
   **Answer:** `"woops mrow 2 mrow haha"`
4. `appedCatty (frappe "blue")` \
   **Answer:** `"woops mrow blue mrow haha"`
5. `cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))` \
   **Answer:** `"pink mrow haha mrow green mrow woops mrow blue"`
6. `cattyConny (flippy "Pugs" "are") "awesome"` \
   **Answer:** `"are mrow Pugs mrow awesome"`

## Recursion
1. Write out the steps for reducing `dividedBy 15 2` to its final answer according to the Haskell code.
   ```haskell
   dividedBy :: (Integral a) => a -> a -> (a, a)
   dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n-d) d (count+1)

   dividedBy 15 2
   => go 15 2 0
   => go (15-2) 2 (0+1)
   => go (13-2) 2 (1+1)
   => go (11-2) 2 (2+1)
   => go (9-2) 2 (3+1)
   => go (7-2) 2 (4+1)
   => go (5-2) 2 (5+1)
   => go (3-2) 2 (6+1)
   => (7, 1)
   ```
2. Write a function that recursively sums all numbers from 1 to n. \
   **Answer** in module `Chapter8`
3. Write a function that multiplies two numbers using recursive summation. \
   **Answer** in module `Chapter8`
