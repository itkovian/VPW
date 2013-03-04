-- Example solution for the balans assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import qualified Data.Set as S
import           Data.List
import           Data.Time.Calendar
import           Data.Time.Format
import           Debug.Trace
import           System.Locale (defaultTimeLocale)

getMyDate s =
    let (d:m:y:_) = words $ map (\c -> if c == '/' then ' ' else c) s
    in fromGregorian (read y) (read m) (read d)

-------------------------------------------------------
-- Solve a single case
solve :: IO String
solve = do
    day <- getMyDate `fmap` getLine

    let passed = diffDays day (fromGregorian 1970 1 1)

        (kinOver, kin') = divMod (5 + passed) 20
        (uinalOver, uinal') = divMod (7 + kinOver) 18
        (tunOver, tun') = divMod (16 + uinalOver) 20
        (katunOver, katun') = divMod (17 + tunOver) 20
        baktun' = 12 + katunOver

    return $ concat . intersperse "." $ reverse $ map show [kin', uinal', tun', katun', baktun']


-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine :: IO Int
    s <- sequence $ replicate n solve

    putStr $ unlines $ s



