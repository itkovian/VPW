-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import qualified Data.Map as M
import           Data.List
import           Debug.Trace

data Colour = Red | Blue | Purple deriving (Show, Eq, Ord)

-------------------------------------------------------
-- Parse a line
parse :: String -> [((Int, Int), Colour)]
parse (c:cs)
    | c == 'R' = zip (fill $ map read . words $ cs) (repeat Red)
    | otherwise = zip (fill $ map read . words $ cs) (repeat Blue)

fill :: [Int] -> [(Int, Int)]
fill (kb:rb:width:height:_) = [(k, r) | k <- (take width [kb ..]), r <- (take height [rb .. ])]


colour :: Colour -> Colour -> Colour
colour c1 c2
    | c1 == c2 = c1
    | otherwise = Purple

-------------------------------------------------------
-- Solve a single case
solve :: IO Int
solve = do
    k <- read `fmap` getLine
    sqs <- liftM concat $ sequence $ replicate k (parse `fmap` getLine)

    return $ length . filter ((== Purple)) $ map (foldl1 colour . map snd) $ groupBy (\x y -> (fst x) == (fst y)) $ sortBy (\x y -> compare (fst x) (fst y)) $ sqs


-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine :: IO Int
    s <- sequence $ replicate n solve

    putStr $ unlines $  map show s


