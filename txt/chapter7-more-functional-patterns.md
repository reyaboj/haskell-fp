# Chapter 7: More Functional Patterns

## Multiple-choice questions
1. A polymorphic function
   1. changes things into sheep when invoked
   2. has multiple arguments
   3. has a concrete type
   4. may resolve to values of different types, depending on inputs :heavy_check_mark:
2. Two functions named `f` and `g` have types `Char -> String` and `String -> [String]` respectively. The composed function `g . f` has the type
   1. `Char -> String`
   2. `Char -> [String]` :heavy_check_mark:
   3. `[[String]]`
   4. `Char -> String -> [String]`
3. A function `f` has type `(Ord a) => a -> a -> Bool` and we apply it to one value; the value obtained has type
   1. `(Ord a) => a -> Bool`
   2. `Num -> Num -> Bool`
   3. `(Ord a) => a -> a -> Integer`
   4. `(Ord a, Num a) => a -> Bool` :heavy_check_mark:
4. A function with type `(a -> b) -> c`
   1. requires values of three different types
   2. is a higher-order function :heavy_check_mark:
   3. must take a tuple as its first argument
   4. has its parameters in alphabetical order
5. Given the following definition of `f`, what is the type of `f True`?
   ```haskell
   f :: a -> a
   f x = x
   ```
   1. `f True :: Bool` :heavy_check_mark:
   2. `f True :: String`
   3. `f True :: Bool -> Bool`
   4. `f True :: a`