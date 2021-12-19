--[># LANGUAGE DeriveGeneric #<]
module Main where

import Data.List.Split
import Data.Maybe
import Data.Foldable (for_)
import qualified Data.HashSet as S
import qualified Data.HashSet (HashSet)
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Unboxed (Vec)
import System.Directory
import System.IO
import Codec.Serialise.IO

import Common
import Affine
import MonoAlphabetic

main :: IO ()
main = do
	hSetBuffering stdout LineBuffering

	exists <- doesFileExist "words-cache.cbor"
	(words, common) <-
		if exists then
			readFileDeserialise "words-cache.cbor"
		else do
			wordsfile  <- readFile "english-words/words.txt"
			commonfile <- readFile "common-words.txt"
			let words  = S.fromList $ splitOn "\n" $ map lower wordsfile
			let common_unf = S.fromList $ splitOn "\n" $ map lower commonfile
			let common = map (\l -> S.filter (\x -> length x == l) common_unf)
				[2..5]

			writeFileSerialise "words-cache.cbor" (words, common)
			return (words, common)

	cypherfile <- readFile "cypher.txt"
	let cypher = map (\c -> if c == '\n' then ' ' else c) cypherfile

	-- print $ crackAffine words cypher

	let alphabet = V.fromList' "qwfpgarstdzxcvbjluyhneiokm"
	let text = map (mas alphabet) "to the and it"
	let alph = fromAlphabet englishAlphabet
	let alphs = filterOnWords words text 2 (cribAttack text ["the", "and"] alph)
	let reverseAlphs = map invertAlphabet alphs

	for_ reverseAlphs (\x -> print $ map (partialMAS x) text)


