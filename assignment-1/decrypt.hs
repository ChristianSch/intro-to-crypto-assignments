--{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Function (on)
import Data.List (sortBy)
import Data.Bits (xor)
import Control.Arrow ((&&&))
import Data.Char (ord)

import Debug.Trace

-- returns the sorted letter frequencies
getSortedLetterFreqDist :: [(B.ByteString, Float)] -> [(B.ByteString, Float)]
getSortedLetterFreqDist = sortBy (compare `on` snd)

-- returns letter frequencies for given letter counts
getLetterFreqDist :: [(B.ByteString, Int)] -> [(B.ByteString, Float)]
getLetterFreqDist freqs = map (\x -> (fst x, (fromIntegral $ snd x) / fromIntegral total)) freqs
    where total = sum $ map snd freqs

-- gets a list of the number of occurrences for each character in the given text as
-- prepared by explodeBase16
letterCount :: B.ByteString -> [(B.ByteString, Int)]
letterCount str = map (L.head &&& L.length) . groupChars $ explodeBase16 str

-- get a list of characters spanning two elements of the byte string each
explodeBase16 :: B.ByteString -> [B.ByteString]
explodeBase16 str = snd $ _explodeBase16 str []

_explodeBase16 :: B.ByteString -> [B.ByteString] -> (B.ByteString, [B.ByteString])
_explodeBase16 str list = case B.length str of
                            0 -> (B.empty, list)
                            _ -> _explodeBase16 (snd rem) (fst rem : list)
                                where rem = B.splitAt 2 str

-- group each ascii char encoded in hex
groupChars :: [B.ByteString] -> [[B.ByteString]]
groupChars list = L.group $ L.sort list


main :: IO ()
main = do
    contents <- B.readFile "ctext.txt"
    print $ getSortedLetterFreqDist . getLetterFreqDist $ letterCount contents
    print $ sum $ map snd (letterCount contents)
    -- this should be round about one
    print $ sum $ map snd (getLetterFreqDist $ letterCount contents)
