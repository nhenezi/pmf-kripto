module Lib
(
  letters, letterFrequencies, alphaEncode, alphaDecode, bigramFrequencies,
  cesarEncode, cesarDecode, vigenerEncode
) where

import Data.Function
import Data.List
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

-- | Letters used in plaintext
letters :: Map Char Integer
letters = Map.fromList $ zipWith (\letter index -> (letter, index)) ['a'..'z'] [0..25]

-- | Converts letter to index
-- We assume that only letters of english alphabet are used
e :: Char -> Integer
e c = case Map.lookup (toLower c) letters of
        Just value -> value
        Nothing -> -1

-- | Converts index to letter
d :: Integer -> Char
d n = case find ((==n).snd) (Map.toList letters) of
        Just value -> fst value
        Nothing -> '0'

-- | Calculates frequency of each letter ordered descending by number of occurances
letterFrequencies :: String -> [(Char, Integer)]
letterFrequencies text = reverse $ sortBy (compare `on` snd) frequencies
  where frequencies = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- text]

-- | Calculates bigram frequency in a text. Ordered descending by number of occurances
bigramFrequencies :: String -> [(String, Integer)]
bigramFrequencies text = reverse $ sortBy (compare `on` snd) bigrams
  where bigrams = concat [(\(x, y) -> ([c, x], y)) <$> (letterFrequenciesAfter c text) | c <- chars]
        chars = fst <$> Map.toList letters

-- | Returs a list of letters that occur after a character in a string
lettersAfter :: Char -> String -> [Char]
lettersAfter c [] = []
lettersAfter c (x:xs)
  | length xs == 0 = []
  | x == c = head xs : lettersAfter c xs
  | otherwise = lettersAfter c xs

-- | Calculates frequency of letters after specific character in a string
letterFrequenciesAfter :: Char -> String -> [(Char, Integer)]
letterFrequenciesAfter c s = letterFrequencies letters
  where letters = lettersAfter c s

-- | Encodes a letter using alpha encoding with (a, b) key
alphaEncodeLetter :: Integer -> Integer -> Char -> Char
alphaEncodeLetter a b c = d $ ((e c) * a + b) `mod` 26

-- | Encodes a string using alpha encoding with key (a, b)
alphaEncode :: Integer -> Integer -> String -> String
alphaEncode a b text = alphaEncodeLetter a b <$> text

-- | Calculates inverse of n in Z_26
inverseMod26 :: Integer -> Maybe Integer
inverseMod26 n = toInteger <$> (elemIndex 1 $ (`mod` 26).(*n) <$> [0..26])

-- | Decodes a single letter in alpha encoding with key (a, b)
alphaDecodeLetter :: Integer -> Integer -> Char -> Maybe Char
alphaDecodeLetter a b c = case inverseMod26 a of
                            Nothing -> Nothing
                            Just val -> Just (d (val * ((e c) - b) `mod` 26))

-- | Decodes a text in alpha encoding with key (a, b)
alphaDecode :: Integer -> Integer -> String -> Maybe String
alphaDecode a b cyphertext = sequence $ alphaDecodeLetter a b <$> cyphertext

-- | Encodes a character using Cesars decoding with key (a)
cesarEncodeLetter :: Integer -> Char -> Char
cesarEncodeLetter a c = d $ ((e c) + a) `mod` 26

-- | Encodes a string using Cesars encryption with key (a)
cesarEncode :: Integer -> String -> String
cesarEncode a text = cesarEncodeLetter a <$> text

-- | Decodes character using Cesars encryption with key (a)
cesarDecodeLetter :: Integer -> Char -> Char
cesarDecodeLetter a c = d $ ((e c) - a) `mod` 26

-- | Decodes string using Cesars encryption with key (a)
cesarDecode :: Integer -> String -> String
cesarDecode a text = cesarDecodeLetter a <$> text

-- | Encodes a string using Vigener cypher
vigenerEncode :: String -> String -> String
vigenerEncode key text = zipWith (\k t -> cesarEncodeLetter (e k) t) expandedKey text
  where
    expandedKey = take (length text) $ cycle key
