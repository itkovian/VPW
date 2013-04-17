-- Example solution for the posters assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import qualified Data.IntMap as M
import           Data.List
import           Debug.Trace

data Poster = Poster Int  -- ^ id
                     Int  -- ^ startsectie
                     Int  -- ^ lengte
              deriving (Show, Eq, Ord)

type Board = M.IntMap Int

-------------------------------------------------------
-- Set the ID of a poster
setId :: Int -> Poster -> Poster
setId k (Poster _ s l) = Poster k s l

-------------------------------------------------------
-- An empty board
emptyBoard :: Board
emptyBoard = M.empty

-------------------------------------------------------
-- Place a poster on a board
place :: Board -> Poster -> Board
place board (Poster k s l) =
    {- trace ("Setting to " ++ show k ++ " from " ++ show s ++ " to " ++ show (s+l-1)) -}$
    foldl' (\b index -> M.alter (\_ -> Just k) index b) board [s .. (s+l-1)]

-------------------------------------------------------
-- How many posters are visible?
visible :: Board -> Int
visible board = length . nub . sort $ M.elems board

-------------------------------------------------------
-- read a poster and give it a default ID
readPoster :: IO Poster
readPoster = do
    s:l:_ <- (map read . words) `fmap` getLine

    return $ Poster 0 s l

-------------------------------------------------------
-- Solve a single case
solve :: IO String
solve = do
    k <- read `fmap` getLine
    posters <- (zipWith setId [1..]) `fmap` replicateM k readPoster

    let board = foldl' place emptyBoard posters

    --print board
    --print $ M.elems board

    return $ show . visible $ board


-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine
    s <- sequence $ replicate n solve

    putStr $ unlines s
