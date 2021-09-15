{-|
Module      : Caeser
Description : A very basic Caeser cipher implementation
License     : BSD-3-Clause
-}
module Cipher where

import qualified Data.Char as Char

{-|
    Applies caeser cipher shift to given string.
-}
caeserCipher :: Int -> String -> String
caeserCipher shift = map (caeserShiftLetter shift)


{-|
    Shifts a single letter in the range a-z by the given shift amount, wrapping around as necessary.
-}
caeserShiftLetter :: Int -> Char -> Char
caeserShiftLetter shift c = shiftLetter shift range c
    where
        shiftLetter k (start,end) c = Char.chr $ wrapAdd start end (Char.ord c) k

        -- Code point bounds for letters in the a-z, A-Z range
        -- We ignore any other characters
        range
            | Char.isAsciiUpper c = (Char.ord 'A', Char.ord 'Z')
            | Char.isAsciiLower c = (Char.ord 'a', Char.ord 'z')
            | otherwise = (Char.ord c, Char.ord c)

        -- Compute x+k, wrapping within the range [a,b] on overflow
        -- Assumes x already in range of [a,b]
        wrapAdd a b x k = a + ( ((x-a) + k) `mod` (b-a+1) )
