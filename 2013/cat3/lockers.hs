-- Example solution for the lockers assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import Control.Monad
import Debug.Trace
import Data.List

data Locker = Locker !Int  -- ^ x
                     !Int  -- ^ y
                     !Int  -- ^ z
                     !(Maybe Bag) -- ^ Empty locker?
                     deriving (Show, Eq, Ord)
data Bag = Bag Int Int Int deriving (Show, Eq, Ord)
data Traveller = Traveller !(Int, Int) -- ^ Arrival time
                           !(Int, Int) -- ^ Train departure time
                           !Int        -- ^ Number of bags
                           ![Bag]      -- ^ Bags
                           deriving (Show, Eq, Ord)


-------------------------------------------------------
-- Read the data for a Locker
readLocker :: IO Locker
readLocker = do
    x:y:z:_ <- (sort . map read . words) `fmap` getLine
    return $ Locker x y z Nothing

-------------------------------------------------------
-- Read the data for a bag
readBag :: IO Bag
readBag = do
    x:y:z:_ <- (sort . map read . words) `fmap` getLine
    return $ Bag x y z


-------------------------------------------------------
-- seconds given a time
minutes :: Int -> Int -> Int
minutes h m = (h * 60) + m

-------------------------------------------------------
-- Read the data for a traveller
readTraveller :: IO Traveller
readTraveller = do
    b       <- read `fmap` getLine
    bh:bm:_ <- (map read . words) `fmap` getLine
    sh:sm:_ <- (map read . words) `fmap` getLine
    bags <- replicateM b readBag
    return $ Traveller (bh, bm) (sh, sm) b bags

-------------------------------------------------------
-- Does the bag fit?
fits :: Bag -> Locker -> Bool
fits (Bag bag_x bag_y bag_z) (Locker lock_x lock_y lock_z _) =
       bag_x <= lock_x
    && bag_y <= lock_y
    && bag_z <= lock_z

-------------------------------------------------------
-- Fill a locker
fill :: Locker -> Bag -> Locker
fill (Locker x y z Nothing) b = Locker x y z (Just b)
fill _ _ = error "Locker already full"

-------------------------------------------------------
-- Store the bags and count the minutes it takes
storeBags :: [Locker] -> [Bag] -> (Bool, [Locker], Int, Int)
storeBags lockers bags = trace ("\narriving with bags " ++ show bags) $ storeBags' ([], lockers) bags 0

storeBags' :: ([Locker], [Locker])        -- ^ The lockers he tried using and the remaining lockers
           -> [Bag]                       -- ^ The bags the traveller carries still
           -> Int                         -- ^ The time he spent
           -> (Bool, [Locker], Int, Int)  -- ^ Successful or not, the new lockers, the time and the bags left
storeBags' (used, lockers) [] m = trace ("no more bags. bailing. used minutes " ++ show m) (True, reverse used ++ lockers, m, 0)  -- zero or more lockers, but no more bags
storeBags' (used, []) bs m = trace ("no more lockers. bailing. used minutes " ++  show m) (False, reverse used, m, length bs)         -- no more lockers to try, still have bags

storeBags' (used, l@(Locker _ _ _ s):lockers) bs m = 
    case s of
        Just _  -> trace ("Locker " ++ show l ++ "full, move to next, minutes " ++ show (m+1)) $ storeBags' (l:used, lockers) bs (m+1)                            -- locker is full, move on
        Nothing -> trace ("(" ++ show l ++ ") empty at minutes " ++ show (m+1)) $ case storeInThisLocker l ([],bs) (m+1) of                           -- locker is free
                        (Just l', m', bs') -> trace ("placed bag in locker: " ++ show l' ++ " at minutes " ++ show m') $ storeBags' (l':used, lockers) bs' m'  -- found a bag that fits
                        (Nothing, m', _)  -> trace ("no bag fits in locker " ++ show l ++ " at minutes " ++ show m') $ storeBags' (l:used, lockers) bs m'    -- no bag fitted

storeInThisLocker :: Locker 
                  -> ([Bag], [Bag])             -- ^ Tried, not fitting and left to try
                  -> Int                        -- ^ Total time
                  -> (Maybe Locker, Int, [Bag]) -- ^ Stored?, Total time, Leftover bags

storeInThisLocker _ (tried, []) m = trace ("tried all bags moving on at minutes " ++ show m)(Nothing, m, reverse tried)   -- tried all bags, move to next locker
storeInThisLocker l (tried, b:bs) m
    | fits b l = trace ("bag " ++ show b ++ " fits in (" ++ show l ++ ") at minutes " ++ show (m+3)) (Just (fill l b), m+3, reverse tried ++ bs)
    | otherwise = trace ("bag " ++ show b ++ " does not fit in (" ++ show l ++ ") at minutes " ++ show (m+2)) $ storeInThisLocker l (b:tried, bs) (m+2)

-------------------------------------------------------
-- Can a traveller store his bags?
store :: ([Locker], [Bool], Int) 
      -> Traveller 
      -> ([Locker], [Bool], Int)    -- ^ New set of lockers, made train, earliest starting time for the next traveller
store (lockers, former, start) (Traveller _ d 0 _) = (lockers, (start < uncurry minutes d):former, start)  -- ^ no bags carried
store (lockers, former, start) (Traveller s d _ bags) =
    let (ok, lockers', spent, left) = storeBags lockers bags
        start' = max start (uncurry minutes s)  -- earliest possible starting time to enter the locker room
        real_time = (uncurry minutes d) - start' - 10 * left in
    trace ("Stored all (" ++ show ok ++ ") spent = " ++ show spent ++ " left = " ++ show left ++ " start' = " ++ show start' ++ " real_time = " ++ show real_time) $
    if spent <= real_time then (lockers', True:former, start' + spent)
                          else (lockers', False:former, start' + spent)

-------------------------------------------------------
-- Solve a single case
solve :: IO String
solve = do
    l <- read `fmap` getLine
    t <- read `fmap` getLine
    lockers <- sequence $ replicate l readLocker
    travellers <- sequence $ replicate t readTraveller

    let (_, solution, _) = foldl store (lockers, [], 0) $ sort travellers

    return $ trace ("------------------------------------------") $ concat . intersperse " " . map (\b -> if b then "JA" else "NEEN") . reverse $ solution


-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine
    s <- sequence $ replicate n solve

    putStr $ unlines $ s
