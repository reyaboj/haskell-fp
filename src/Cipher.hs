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

-- | Computes the inclusive range of codepoints based on whether the given character is upper/lowercase. For any other characters, we have the range covering only that character.
-- Example 1: If the character is 'c' then the range is for ascii lowercase: (97,122)
-- Example 2: If the character is ';' then the range is (59,59)
charRange :: Char -> (Int, Int)
charRange c
    | Char.isAsciiUpper c = (Char.ord 'A', Char.ord 'Z')
    | Char.isAsciiLower c = (Char.ord 'a', Char.ord 'z')
    | otherwise = (Char.ord c, Char.ord c)


{-|
    Shifts a single letter in the range a-z (lower or uppercase) by the given shift amount, wrapping around as necessary.
-}
caeserShiftLetter :: Int -> Char -> Char
caeserShiftLetter shift c = shiftLetter shift window c
    where
        shiftLetter k (start,end) c = Char.chr $ wrapAdd start end (Char.ord c) k

        window = charRange c

        -- Compute x+k, wrapping within the range [a,b] on overflow
        -- Assumes x already in range of [a,b]
        wrapAdd a b x k = a + ( ((x-a) + k) `mod` (b-a+1) )


{- Vigenere cipher -}

-- | Keyword is a string determining the shift amount at each position
-- Example: keyword "ABZ" with plaintext "abcdefgh" is matched as follows
--          abcdefgh
--          ABZABZAB
-- In other words, the keyword is cyclically repeated as often as needed to pad our shift string.
-- Ignores any non-alphabet (A-Z,a-z) character.
type Keyword = String

vigenereCipher :: Keyword -> String -> String
vigenereCipher keyword plaintext = ciphertext
    where
        ciphertext = map (uncurry caeserShiftLetter) charShiftPairs
        charShiftPairs = zip shifts plaintext
        shifts = map charOffset $ cycle keyword
        charOffset c = Char.ord c - fst (charRange c)