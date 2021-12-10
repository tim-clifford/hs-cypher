module Main where

import Lib
import Data.List.Split
import Data.Char (ord, chr)
import Data.Sort
import Data.Typeable

main :: IO ()
main = do
	file <- readFile "english-words/words.txt"
	let words = splitOn "\n" $ map lower file
	cipherfile <- readFile "cypher.txt"
	let cipher = map (\c -> if c == '\n' then ' ' else c) cipherfile
	print $ crackAffine words cipher

crackAffine words ciphertext = head $
	dropWhile (\x -> checkEnglish words x < 0.5)
		(map (\x -> map (reverseAffine (fst x) (snd x)) ciphertext)
			(sortLinear reverseAffine affineAs [0..25] ciphertext))


checkEnglish :: [[Char]] -> [Char] -> Float
checkEnglish words text = (foldl (\x w -> if w `elem` words then (x+1) else (x)) 0 textWords)
		 / (fromIntegral $ length textWords)
	where textWords = splitOn " " $ map lower text

sortLinear :: Num a => Num b => (a -> b -> Char -> Char) -> [a] -> [b] -> String -> [(a, b)]
sortLinear f as bs ciphertext = reverse $ sortOn
	(\(a,b) -> likelihood $ map (f a b) ciphertext)
	[(a, b) | a <- as, b <- bs]

letterFrequency = [0.0817, 0.0149, 0.0278, 0.0425, 0.127, 0.0223, 0.0202, 0.0609, 0.0697, 0.0015, 0.0077, 0.0402, 0.0241, 0.0675, 0.0751, 0.0193, 0.0009, 0.0599, 0.0633, 0.0906, 0.0276, 0.0098, 0.0236, 0.0015, 0.0197, 0.0007]

likelihood :: [Char] -> Float
likelihood cs = sum $ zipWith (*) letterFrequency (getFrequencies cs)

getFrequencies :: String -> [Float]
getFrequencies str = map
	(\x -> (fromIntegral (length (filter (==x) str))) / (fromIntegral (length str)))
	"abcdefghijklmnopqrstuvwxyz"

isLower x = 97 <= x && x <= 122
isUpper x = 65 <= x && x <= 95

lower c
	| isUpper x = chr $ x + 32
	| otherwise = c
	where x = ord c

upper c
	| isLower x = chr $ x - 32
	| otherwise = c
	where x = ord c

affineAs = [1,3,5,7,9,11,15,17,19,21,23,25]
affine :: Int -> Int -> Char -> Char
affine a b c
	| isLower x = chr (((x - 97)*a + b) `mod` 26 + 97)
	| isUpper x = chr (((x - 65)*a + b) `mod` 26 + 65)
	| otherwise = c
	where x = ord c

reverseAffine a b c
	| isLower x = chr (((x - 97 - b)*a') `mod` 26 + 97)
	| isUpper x = chr (((x - 65 - b)*a') `mod` 26 + 65)
	| otherwise = c
	where x = ord c
	      a' = head $ filter (\x -> x*a `mod` 26 == 1) affineAs
