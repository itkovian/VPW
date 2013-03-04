-- Example solution for the balans assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import           Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List
import           Debug.Trace


transform :: S.Set Int -> Int -> S.Set Int -> S.Set Int -> Bool
transform all t av s
    | S.member t s = True
    | S.null s' = False
    | otherwise = transform all t av $ foldl (flip S.insert) S.empty [v1 + v2 | v1 <- S.elems av, v2 <- S.elems s', v1 /= v2, v1 + v2 <= t, S.member (v1+v2) all]
  where s' = S.filter (<= t) s



-------------------------------------------------------
-- Solve a single case
solve :: M.Map String Int -> IO Bool
solve m = do
    available <- words `fmap` getLine
    targets <- words `fmap` getLine

    let av = S.fromList $ map (\e -> fromJust $ M.lookup e m) available
        ts = map (\e -> fromJust $ M.lookup e m) targets
        all = S.fromList $ M.elems m


    return $ and $ map (\t -> transform all t av av) ts

-------------------------------------------------------
-- Main, duh.
main = do
    e <- read `fmap` getLine :: IO Int
    elems <- fmap M.fromList $ sequence $ replicate e $ ((\(e:v:_) -> (e, read v)) . words) `fmap` getLine
    n <- read `fmap` getLine
    s <- sequence $ replicate n (solve elems)

    putStr $ unlines $ map (\b -> if b then "ja" else "neen") s


