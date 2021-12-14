{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
--module MonoAlphabetic (crackMAS, mas, invertAlphabet) where
module MonoAlphabetic where

import Data.Char (ord, chr)
import Data.Sort
import Data.Maybe
import Data.List.Split
import Data.List (nub)
import Control.Monad
import Data.Function
import Data.Functor.Identity (Identity(..))
import qualified Data.HashSet as S
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Unboxed (Vec)

import Common

crackMAS :: [Char] -> [Char]
crackMAS cyphertext = "Not implemented"

-- might break if they're not a-z
mas :: Alphabet -> Char -> Char
mas alph c
	| isLowerAsc x = alph V.! (x - 97)
	| isUpperAsc x = upper $ alph V.! (x - 65)
	| otherwise = c
	where x = ord c

partialMAS :: AnnotatedAlphabet -> Char -> Char
partialMAS alph c
	| isNothing x = c
	| snd c' = lower (fst c')
	| otherwise = upper c
	where x  = char26 c
	      c' = alph V.! (fromMaybe (-1) x)

invertAlphabet :: AnnotatedAlphabet -> AnnotatedAlphabet
invertAlphabet alph =
	foldl (\alph x ->
		let k = char26 (fst (fst x)) in
		if snd (fst x) && isJust k then
			unsafeSet (fromJust k) (chr ((snd x) + 97), True) alph
		else
			alph
	) (fromAlphabet englishAlphabet) (zip (V.toList alph) [0..25])

	--zip (map lower $ V.toList alph) (V.toList englishAlphabet)
	-- & sort & map snd & V.fromList'

-- True denotes a character that has been changed. Another option would be to
-- use capitals but I want this to be extendable to other character sets
type AnnotatedAlphabet = Vec 26 (Char, Bool)

fromAlphabet :: Alphabet -> AnnotatedAlphabet
fromAlphabet = V.map (,False)

toAlphabet :: AnnotatedAlphabet -> Alphabet
toAlphabet = V.map fst

cribAttack :: [Char] -> [[Char]] -> AnnotatedAlphabet -> [AnnotatedAlphabet]
cribAttack _ [] _ = [fromAlphabet englishAlphabet]
cribAttack cyphertext cribs alph =
	catMaybes [
		joinAlphabets a b |
			-- not sure why i couldn't pattern match head and tail
			a <- allPossibleAlphs cyphertext (head cribs) alph,
			b <- cribAttack       cyphertext (tail cribs) alph
	]

allPossibleAlphs :: [Char] -> [Char] -> AnnotatedAlphabet -> [AnnotatedAlphabet]
allPossibleAlphs cyphertext textword alph =
	mapMaybe (\x -> addWordToAlphabet alph x textword)
	         (allPossibleMatches cyphertext textword alph)

allPossibleMatches :: [Char] -> [Char] -> AnnotatedAlphabet -> [[Char]]
allPossibleMatches cyphertext textword alph =
		splitOn " " (map lower cyphertext)
		& filter (\x -> l == length x)
		& filter (\w -> or $ zipWith (\cc ct ->
				isAlphChar ct && (fst (alph V.! fromJust (char26 ct)) /= cc))
			w textword)
		& nub
	where l = length textword

addWordToAlphabet :: AnnotatedAlphabet -> [Char] -> [Char] -> Maybe AnnotatedAlphabet
addWordToAlphabet alph cypher text
	| isNothing wordAlph = Nothing
	| otherwise = joinAlphabets alph $ fromJust wordAlph
	where wordAlph = alphFromWord cypher text

joinAlphabets :: AnnotatedAlphabet -> AnnotatedAlphabet -> Maybe AnnotatedAlphabet
joinAlphabets a b =
	if isJust joined && (length $ S.fromList $ V.toList $ fromJust joined) == 26
	then
		joined
	else
		Nothing
	where {
		joined = V.zipWithM (\a b ->
			if snd a == snd b then
				if fst a == fst b then
					Just a
				else
					Nothing
			else if snd a then
				Just a
			else if snd b then
				Just b
			else
				Nothing -- this can never happen
	) a b }

-- can probably relax some isAlphs at some point
alphFromWord :: [Char] -> [Char] -> Maybe AnnotatedAlphabet
alphFromWord cypher text = foldM (\alph x ->
	let {cc = fst x;
	     ct = snd x;
		 k  = fromMaybe (-1) (char26 ct);
		 ca = (alph V.! k);} in
	if (length cypher /= length text) || (snd ca && fst ca /= cc) then
		Nothing
	else if k == -1 then
		Just alph
	else
		Just (unsafeSet k (cc, True) alph)
	) (fromAlphabet englishAlphabet) (zip cypher text)

unsafeSet :: Int -> (Char, Bool) -> AnnotatedAlphabet -> AnnotatedAlphabet
unsafeSet k a = runIdentity . V.element k (const (Identity a))
