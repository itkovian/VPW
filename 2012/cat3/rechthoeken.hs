{-
 - (C) 2012, Andy Georges
 -
 - Oplossing voor het veelhoekenprobleem van de VPW 2012.
 - http://vlaamseprogrammeerwedstrijd.be
 -
 -}

module Main where

import Control.Monad
import Data.Either
import Data.List ((\\), groupBy, intersect, iterate, nub, sort, takeWhile)
import Data.Monoid
import Data.Maybe (catMaybes, isJust)

data Line = Line 
          { level :: Int
          , begin :: Int
          , end :: Int
          } deriving (Eq, Show)

instance Ord Line where
  compare l1 l2 
    | (level l1) < (level l2) = LT
    | (level l1) > (level l2) = GT
    | (begin l1) < (begin l2) = LT
    | (begin l1) > (begin l2) = GT
    | (end l1) < (end l2) = LT
    | (end l1) > (end l2) = GT
    | otherwise = EQ

isLevel :: Line -> Line -> Bool
isLevel l1 l2 = (level l1) == (level l2)

-- | expect: level to be equal
contains :: Line -> Line -> Bool
contains l1 l2
  | l1 <= l2 = (begin l1) <= (begin l2) && (end l1) >= (end l2)
  | otherwise = False

-- | expect: level to be equal
overlaps :: Line -> Line -> Bool
overlaps l1 l2
  | l1 <= l2 = (end l1) >= begin (l2)
  | otherwise = overlaps l2 l1

-- | Check!
combine :: Line -> Line -> Maybe Line
combine l1 l2
  | not $ isLevel l1 l2 = Nothing
  | l1 `contains` l2 = Just l1
  | l2 `contains` l1 = Just l2
  | l1 > l2 = combine l2 l1
  | l1 `overlaps` l2 = Just $ l1 { end = (end l2) }
  | otherwise = Nothing

-----------------------------------------------------------------
-- helper functies
recombine :: [Line] -> [Line]
recombine [] = []
recombine [l] = [l]
recombine (l1:l2:ls) =
    case combine l1 l2 of
      Nothing -> l1 : recombine (l2:ls)
      Just l -> recombine (l:ls)

combining :: [Line] -> [Line]
combining ls = 
    let sls = sort ls -- per level, then from left to right or bottom to top
        gls = groupBy (\l1 l2 -> level l1 == level l2) sls -- so we combine on a per-level basis
        cls = map recombine gls -- make larger lines
    in concat cls

empty :: Line -> Bool
empty l = (begin l) == (end l)

intersects :: Line -> Line -> Bool
intersects l1 l2 =  (level l1) `inbetween` (begin l2, end l2) 
                 && (level l2) `inbetween` (begin l1, end l1)
  where v `inbetween` (x, y) = x <= v && v <= y 

split :: Line -> Line -> (Line, Line)
split l1 l2 = (l1 { end = level l2 }, l1 { begin = level l2 })

match :: Line -> Line -> Bool
match l1 l2 = (begin l1) == (begin l2) && (end l1) == (end l2)

intersections :: Line -> [Line] -> [Line]
intersections l [] = [l]
intersections l ilss@(il:ils)
    | intersects l il = let (s1, s2) = split l il
                        in if empty s1 
                              then intersections l ils
                              else s1 : (intersections s2 ils) ++ (intersections l ils)
    | otherwise = intersections l ils

-----------------------------------------------------------------
-- Oplossing
square :: (Line, Line) -> (Line, Line) -> Bool
square (l1x, l2x) (l1y, l2y) = and $ [ cornerMatch l2x l2y
                                     , cornerMatch l2x l2y
                                     , cornerMatch l1x l1y
                                     , cornerMatch l1x l2y
                                     ]
  where cornerMatch lx ly = not . null $ cornersX lx `intersect` cornersY ly
        cornersX (Line l b e) = [(b,l), (e,l)]
        cornersY (Line l b e) = [(l,b), (l,e)]

-- | this works by construction of the argument tuples
square' :: (Line, Line) -> (Line, Line) -> Bool
square' (Line lx_y1 lx_x1 lx_x2, Line lx_y2 _ _) (Line ly_x1 ly_y1 ly_y2, Line ly_x2 _ _) =
       lx_y1 == ly_y1
    && lx_y2 == ly_y2
    && ly_x1 == lx_x1
    && ly_x2 == lx_x2

count :: [Line] -> [Line] -> String
count xls yls =
    let xls' = combining xls
        yls' = combining yls
        xs = map (sort . nub . flip intersections yls') xls'
        ys = map (sort . nub . flip intersections xls') yls'
        sameXs = nub $ [(l1, l2) | l1s <- xs, l2s <- xs, l1s /= l2s, l1 <- l1s, l2 <- l2s, l1 <= l2, match l1 l2]
        sameYs = nub $ [(l1, l2) | l1s <- ys, l2s <- ys, l1s /= l2s, l1 <- l1s, l2 <- l2s, l1 <= l2, match l1 l2]
        sqs = nub $ filter (uncurry square') $ [(xp, yp) | xp <- sameXs, yp <- sameYs]
    in show $ length sqs

-----------------------------------------------------------------
-- Parse the data
parseLine :: String -> Either Line Line
parseLine s = 
    let x1:y1:x2:y2:_ = map read $ words s
    in if x1 == x2 
          then Right (Line { level = x1, begin = y1, end = y2 })
          else Left (Line { level = y1, begin = x1, end = x2 })

-----------------------------------------------------------------
-- I/O shizzle
solve :: IO String
solve = do
    l <- read `fmap` getLine
    ls <- replicateM l (parseLine `fmap` getLine)
    let xs = lefts ls
        ys = rights ls
    return $ count xs ys

main = do 
    n <- read `fmap` getLine
    s <- sequence $ replicate n solve
    mapM putStrLn s
