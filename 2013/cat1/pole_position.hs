-- Example solution for the pole_position assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import qualified Data.IntMap as M
import           Data.List
import           Debug.Trace

data Car = Car Int -- ^ID
               Int -- ^Current position
               Int -- ^Position change
               Int -- ^Original position
               deriving (Show, Eq)

instance Ord Car where
    (Car _ _ _ p) <= (Car _ _ _ q) = p <= q
    (Car _ _ _ p) > (Car _ _ _ q) = p > q

-------------------------------------------------------
-- verify that all start positions are unique
verify :: Int -> [Car] -> Bool
verify a cs =
    let orig = map (\(Car _ _ _ o) -> o) cs
    in a == (length . nub $ orig) && (null $ filter (\p -> p < 1 || p > a) orig)

-------------------------------------------------------
-- determine original ordering
order :: [Car] -> [Int]
order cs = map (\(Car i _ _ _) -> i) $ sort cs


-------------------------------------------------------
-- Solve a single case
solve :: IO (Maybe [Int])
solve = do
    a <- read `fmap` getLine  -- number of cars
    cars <- fmap (zipWith (\i s -> (\(w:p:_) -> Car (read w) i (read p) (i + (read p))) $ words s) [1..]) $ sequence $ replicate a getLine

    return $ case verify a cars of
                True -> Just $ order cars
                False -> Nothing

-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine :: IO Int
    s <- sequence $ replicate n solve

    putStr $ unlines $ map show s

