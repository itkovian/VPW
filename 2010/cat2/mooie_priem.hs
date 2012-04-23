import Data.List
import Data.Maybe
import System.IO
import Control.Applicative

data Prime = P Int | Unknown 

instance Show Prime where
  show (P i) = show i
  show Unknown = "geen priemgetal gevonden"


isPrime :: Int -> Bool
isPrime n 
  | n == 1 = False
  | n == 2 || n == 3 || n == 5 = True
  | otherwise = (n `mod` 2 /= 0) && (n `mod` 3 /= 0) && 
                (case (n `mod` 10) `elem` [1,3,7,9] of
                 False -> False
                 True -> let upperBound = 1 + (round . sqrt $ fromIntegral n)
                             divisors = takeWhile (< upperBound) $ concat [[6*k - 1, 6*k + 1] | k <- [1..]]
                         in and $ map (\d -> n `mod` d /= 0) divisors)
    

determineNearest :: [(Int, Int)] -> Prime
determineNearest [] = Unknown 
determineNearest ps = 
  let (p, d) = head $ sortBy (\(p,d) (p', d') -> case compare d d' of 
                                                   EQ -> compare p p'
                                                   c -> c) ps
  in P p

walkPowerToPrime :: Int -> Int -> Int -> Maybe (Int, Int)
walkPowerToPrime m n power =
  let higherP = filter isPrime $ [(max power m) .. n]
      lowerP = filter isPrime $ [(min power n), (min power n) - 1 .. m]
  in case (higherP, lowerP) of
    ([], []) -> Nothing
    (h:_, []) -> Just (h, abs (h - power))
    ([] , l:_) -> Just (l, abs (l - power))
    (h:_, l:_) -> let dh = abs (h - power)
                      dl = abs (l - power)
                  in if dl <= dh then Just (l, dl)
                     else Just (h, dh)

powers :: Int -> Int -> [Int]
powers m n = 
  let logM = (round . logBase 2) (fromIntegral m) 
      logN = (round . logBase 2) (fromIntegral n)
      ks = filter (>= 0) [(logM - 1) .. (logN + 1)] 
  in [2^k | k <- ks]


primes :: Int -> Int -> Prime
primes m n = 
  let ps = catMaybes $ map (walkPowerToPrime m n) $ powers m n
  in determineNearest ps

main = do 
  t <- read <$> getLine :: IO Int
  lss <- lines <$> getContents

  let ps = map (uncurry primes . (\(m:n:_) -> (m,n)) . map read . words) $ take t lss 

  putStr $ unlines $ map show ps
