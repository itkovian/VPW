{-# LANGUAGE BangPatterns #-}
-- Example solution for the cocktails assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Debug.Trace

-------------------------------------------------------
-- Find a combination of bartenders

shake :: [String] -> String -> String -> Bool
shake tenders all cocktails 
    | not . null $ filter (flip notElem all) cocktails = False
    | otherwise = let required = filter (not . null) $ map (filter (flip elem cocktails)) tenders
                  in case singletonFilter required cocktails of
                        ([], []) -> trace (show required) True
                        ([], _) -> trace (show required) False
                        (required', cocktails') -> 
                            let couples = [ (c, i) | (i, cs) <- zip [1..] required', c <- cs, c `elem` cocktails']
                                choiceMap =  M.fromList $ map (\bs -> (fst $ head bs, map snd bs)) $ groupBy (\(c1, _) (c2, _) -> c1 == c2) $ sort couples
                            in trace ("choiceMap = " ++ show choiceMap) $ possible (length cocktails') $ sortBy listLength $ map (\k -> fromJust $ M.lookup k choiceMap) cocktails'

listLength :: [a] -> [a] -> Ordering
listLength xs ys =  compare (length xs) (length ys)
{-# INLINE listLength #-}

singletonFilter :: (Eq a, Show a) => [[a]] -> [a] -> ([[a]], [a])
singletonFilter xss cs =
    let (singletons, reduced) = partition (\xs -> case xs of _:[] -> True; _ -> False) xss
    in if null singletons then (xss, cs)
                          else singletonFilter reduced (clear (concat singletons) cs)

clear :: (Eq a, Show a) => [a] -> [a] -> [a]
clear [] cs = cs
clear (s:ss) cs = clear ss $
    case break (== s) cs of
        (p, []) -> {- trace ("removed " ++ show s ++ " remainder = " ++ show p) -} p
        (p, s') -> {- trace ("removed " ++ show s ++  " remainder = " ++ show (p ++ tail s')) $ -} p ++ (tail s')


possible :: Int -> [[Int]] -> Bool
possible 0 _ = True
possible _ [] = False
possible _ ([]:_) = False
possible !l iss@((i:is):iss')
    | length iss < l = False
    | otherwise =  let reduced = sortBy listLength $ remove i iss'
                   in (trace ("deeper to " ++ show (l-1)) $ possible (l - 1) reduced)
                      || (trace ("sibling at " ++ show l)$ possible l (is:iss'))


remove :: Int -> [[Int]] -> [[Int]]
remove x = filter (not . null) . map (filter (/= x))


-------------------------------------------------------
-- Solve a single case
solve :: IO [(String,Bool)]
solve = do
    b <- read `fmap` getLine
    bss <- replicateM b getLine
    k <- read `fmap` getLine
    cs <- replicateM k getLine

    let shakes = map (shake bss (nub $ concat bss)) cs 
    
    return $ zip cs shakes

display :: (String, Bool) -> String
display (s, True) = s ++ " mogelijk"
display (s, False) = s ++ " onmogelijk"

-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine
    s <- replicateM n solve

    putStr $ unlines $ concatMap (map display) s
