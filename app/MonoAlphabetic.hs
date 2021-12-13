--module MonoAlphabetic (crackMAS, mas, reverseMAS) where
module MonoAlphabetic where

import Data.Char (ord, chr)
import Data.Sort
import Data.Function
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Boxed (Vec)
import qualified Data.HashSet as S

import Common

-- might break if they're not a-z
mas :: Alphabet -> Char -> Char
mas alph c
	| isLower x = alph V.! (x - 97)
	| isUpper x = upper $ alph V.! (x - 65)
	| otherwise = c
	where x = ord c

invertAlphabet :: Alphabet -> Alphabet
invertAlphabet alph =
	zip (map lower $ V.toList alph) (V.toList englishAlphabet)
	& sort & map (\x -> snd x) & V.fromList'

