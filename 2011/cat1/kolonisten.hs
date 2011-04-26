module Main where

import Data.List (groupBy, sort, sortBy)
import Data.Maybe (fromJust)

odds :: [(Int, Double)]
odds = map (\(v, c) -> (v, c/36.0))
     [ (1,0)
     , (2,1)
     , (3,2)
     , (4,3)
     , (5,4)
     , (6,5)
     , (7,6)
     , (8,5)
     , (9,4)
     , (10,3)
     , (11,2)
     , (12,1)
     ]
  

colonise :: [String] -> [String]
colonise [] = []
colonise (cs:rs:ds:lss) = weakest (read cs) (map head $ words rs) (map read $ words ds) : colonise lss


weakest :: Int -> String -> [Int] -> String
weakest c rs ds = 
  let gs = groupBy (\(r1, _) (r2, _) -> r1 == r2) . sort $ zip rs ds 
      os = map (addOdds . map (\(r, d) -> (r, fromJust $ lookup d odds))) gs
      sos = sortBy (\(_, o1) (_, o2) -> compare o1 o2) os
  in case sos of
    (r, o):[] -> [r]
    (r1, o1):(r2,o2):_ -> if o1 == o2 then 
                            let cr1 = length $ filter ((==) r1) rs
                                cr2 = length $ filter ((==) r2) rs
                            in if cr1 < cr2 then [r1] else [r2]
                          else [r1]
  
  where addOdds :: [(Char, Double)] -> (Char, Double)
        addOdds oss@((r, _):_) = (r, sum $ map snd oss) 


main = do
  lss <- lines `fmap` getContents
  let cases = read $ head lss :: Int
      resources = colonise $ tail lss

  putStr $ unlines  resources
