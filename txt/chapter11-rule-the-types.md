# Chapter 11

## 11.4 The Quad

```haskell
data Quad =
    One
    | Two
    | Three
    | Four
    deriving (Eq, Show)
```

How many different forms for each of the following?

```haskell
-- (1)
eQuad :: Either Quad Quad

-- (2)
prodQuad :: (Quad, Quad)

-- (3)
funcQuad :: Quad -> Quad

-- (4)
prodTBool :: (Bool, Bool, Bool)

-- (5)
gTwo :: Bool -> Bool -> Bool

-- (6)
fTwo :: Bool -> Quad -> Quad
```
**Note:** We use the notation #(`X`) to denote the cardinality of type `X`, or the cardinality of the type of some binding `X`.

We know #(`Quad`) = 1 + 1 + 1 + 1 = 4, because of its four nullary constructors.

We know that for a sum type `T = A | B`, #(`T`) = #(`A`) + #(`B`).

For a product type, `T = A B`, #(`T`) = #(`A`) * #(`B`).

For a function type `a -> b`, #(`a -> b`) = #(`b`) ^ #(`a`), where ^ is exponentiation.

1. #(`eQuad`) = #(`Either Quad Quad`) \
   #(`Either Quad Quad`) = #(`Left Quad`) + #(`Right Quad`) \
   #(`Left Quad`) = #(`Quad`) = 4 \
   #(`Right Quad`) = #(`Quad`) = 4 \
   Therefore, #(`eQuad`) = 4 + 4 = 8
2. #(`prodQuad`) = #( `(Quad, Quad)` ) \
   #( `(Quad, Quad)` ) = #(`Quad`) * #(`Quad`) = 4 * 4 = 16
   Therefore, #(`prodQuad`) = 16
3. #(`funcQuad`) = #(`Quad`) ^ #(`Quad`) = 4^4 = 256
4. #(`prodTBool`) = #(`Bool`) * #(`Bool`) * #(`Bool`) = 2 * 2 * 2 = 8
5. #(`gTwo`) = #(`Bool -> Bool -> Bool`) \
   #(`Bool -> Bool -> Bool`) = #(`Bool -> Bool`) ^ #(`Bool`) \
   #(`Bool -> Bool`) = 2^2, so that #(`Bool -> Bool -> Bool`) = (2^2)^2 = 16
6. #(`fTwo`) = #(`Bool -> Quad -> Quad`) \
   #(`Bool -> Quad -> Quad`) = #(`Quad -> Quad`) ^ #(`Bool`) \
   #(`Quad -> Quad`) = 4^4, so we get #(`Bool -> Quad -> Quad`) = (4^4) ^ 2 = 2^16

## Chapter Exercises
1. Given the datatype below, pick the correct answer.
   ```haskell
   data Weekday =
      Monday
      | Tuesday
      | Wednesday
      | Thursday
      | Friday
   ```
   1. `Weekday` is a type with five data constructors :heavy_check_mark:
   2. `Weekday` is a tree with five branches
   3. `Weekday` is a product type
   4. `Weekday` takes five arguments
2. With the same datatype as above, what is the type of `f`?
   ```haskell
   f Friday = "Miller Time"
   ```
   1. `f :: [Char]`
   2. `f :: String -> String`
   3. `f :: Weekday -> String` :heavy_check_mark:
   4. `f :: Day -> Beer`
3. Types defined with the `data` keyword
   1. must have at least one argument
   2. must begin with a capital letter :heavy_check_mark:
   3. must be polymorphic
   4. cannot be imported from modules
4. The function `g xs = xs !! (length xs - 1)`
   1. is recursive and may not terminate
   2. delivers the head of `xs`
   3. delivers the final element of `xs` :heavy_check_mark:
   4. has the same type as `xs`

