module Main where

import Data.Char (ord, chr)
import Data.List (group)

dna :: String -> String
dna s = 
  let orig:compressed:[] = words s
  in case orig of
    "???" -> concat [decompress compressed, " ", compressed]
    _ -> concat [orig, " ", compress orig]


compress :: String -> String
compress s = 
  let gs = group s
  in concat $ map grouprepresentation gs

grouprepresentation :: String -> String
grouprepresentation s =
    if length s >= 4 then
      let (cs, ts) = splitAt 26 s
      in '-':chr (ord 'A' + length cs - 1):head cs: grouprepresentation ts
    else s


decompress :: String -> String
decompress [] = []
decompress (c:s) = 
  case c of
    '-' -> let count:symbol:ss = s
           in (replicate (ord count - ord 'A' + 1) symbol) ++ decompress ss
    _ -> c : decompress s


main = do
  lss <- lines `fmap` getContents

  putStr $ unlines $ map dna  $ tail lss