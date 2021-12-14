{-# LANGUAGE DataKinds #-}
module Common where

import Data.Char (ord, chr)
import Data.List.Split
import qualified Data.HashSet as S
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Unboxed (Vec)

infixl 7 /.
(/.) :: (Real a, Real b, Fractional c) => a -> b -> c
x /. y = realToFrac x / realToFrac y

type Alphabet = Vec 26 Char

englishAlphabet :: Alphabet
englishAlphabet = V.fromList' "abcdefghijklmnopqrstuvwxyz"

letterFrequency = [0.0817, 0.0149, 0.0278, 0.0425, 0.127, 0.0223, 0.0202, 0.0609, 0.0697, 0.0015, 0.0077, 0.0402, 0.0241, 0.0675, 0.0751, 0.0193, 0.0009, 0.0599, 0.0633, 0.0906, 0.0276, 0.0098, 0.0236, 0.0015, 0.0197, 0.0007]

checkEnglish :: RealFrac a => S.HashSet [Char] -> a -> [Char] -> Bool
checkEnglish words thresh text = matchToThreshold
		(floor (thresh * fromIntegral (length textWords)))
		(`S.member` words)
		textWords
	where textWords = splitOn " " $ map lower text

matchToThreshold :: Int -> (a -> Bool) -> [a] -> Bool
matchToThreshold n _ _
	| n <= 0 = True
matchToThreshold n _ [] = False
matchToThreshold n p (x:xs) =
  matchToThreshold (if p x then n-1 else n) p xs

likelihood :: [Char] -> Float
likelihood cs = sum $ zipWith (*) letterFrequency (getFrequencies cs)

getFrequencies :: String -> [Float]
getFrequencies str = map
	(\x -> length (filter (==x) str) /. length str)
	(V.toList englishAlphabet)

isLowerAsc :: (Integral t) => t -> Bool
isLowerAsc x = 97 <= x && x <= 122

isUpperAsc :: (Integral t) => t -> Bool
isUpperAsc x = 65 <= x && x <= 95

isAlphChar :: Char -> Bool
isAlphChar c = isUpperAsc x || isLowerAsc x
	where x = ord c

lower :: Char -> Char
lower c
	| isUpperAsc x = chr $ x + 32
	| otherwise = c
	where x = ord c

upper :: Char -> Char
upper c
	| isLowerAsc x = chr $ x - 32
	| otherwise = c
	where x = ord c

char26 :: Char -> Maybe Int
char26 c
	| isLowerAsc x = Just (x - 97)
	| isUpperAsc x = Just (x - 65)
	| otherwise = Nothing
	where x = ord c
