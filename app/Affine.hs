module Affine (crackAffine, affine, reverseAffine) where

import Data.Char (ord, chr)
import Data.Sort
import Data.Function
import qualified Data.HashSet as S

import Common

crackAffine :: S.HashSet [Char] -> [Char] -> [Char]
crackAffine words ciphertext =
	sortLinear reverseAffine affineMultipliers [0..25] ciphertext
	& map (\x -> map (uncurry reverseAffine x) ciphertext)
	& filter (checkEnglish words 0.5)
	& head

sortLinear :: (Num a, Num b) => (a -> b -> Char -> Char) -> [a] -> [b] -> String -> [(a, b)]
sortLinear f as bs ciphertext = reverse $ sortOn
	(\(a,b) -> likelihood $ map (f a b) ciphertext)
	[(a, b) | a <- as, b <- bs]

affineMultipliers = [1,3,5,7,9,11,15,17,19,21,23,25]

affine :: Int -> Int -> Char -> Char
affine a b c
	| isLowerAsc x = chr (((x - 97)*a + b) `mod` 26 + 97)
	| isUpperAsc x = chr (((x - 65)*a + b) `mod` 26 + 65)
	| otherwise = c
	where x = ord c

reverseAffine :: Int -> Int -> Char -> Char
reverseAffine a b c
	| isLowerAsc x = chr (((x - 97 - b)*a') `mod` 26 + 97)
	| isUpperAsc x = chr (((x - 65 - b)*a') `mod` 26 + 65)
	| otherwise = c
	where x = ord c
	      a' = head $ filter (\x -> x*a `mod` 26 == 1) affineMultipliers
