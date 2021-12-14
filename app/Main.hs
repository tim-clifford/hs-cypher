module Main where

import Data.List.Split
import Data.Maybe
import qualified Data.HashSet as S
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Unboxed (Vec)

import Common
import Affine
import MonoAlphabetic

main :: IO ()
main = do
	file <- readFile "english-words/words.txt"
	let words = S.fromList $ splitOn "\n" $ map lower file
	cypherfile <- readFile "cypher.txt"
	let cypher = map (\c -> if c == '\n' then ' ' else c) cypherfile
	-- print $ crackAffine words cypher
	let alphabet = V.fromList' "qwfpgarstdzxcvbjluyhneiokm"
	let text = map (mas alphabet) "to the and it"
	print text
	let alph = fromAlphabet englishAlphabet
	let alphs = cribAttack text ["the", "and"] alph
	print $ map (\x -> map (partialMAS $ invertAlphabet x) text) (cribAttack text ["the", "and"] alph)
