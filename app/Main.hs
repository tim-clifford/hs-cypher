module Main where

import Data.List.Split
import qualified Data.HashSet as S
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Boxed (Vec)

import Common
import Affine
import MonoAlphabetic

main :: IO ()
main = do
	file <- readFile "english-words/words.txt"
	let words = S.fromList $ splitOn "\n" $ map lower file
	cipherfile <- readFile "cypher.txt"
	let cipher = map (\c -> if c == '\n' then ' ' else c) cipherfile
	-- print $ crackAffine words cipher
	let alphabet = V.fromList' "qwfpgarstdzxcvbjluyhneiokm"
	print $ map (mas alphabet) "Hello Wrold"
	print $ map (mas $ invertAlphabet alphabet) "Sgxxb Iubxp"
