{- (C) 2010 Andy Georges
 -
 - Oplossing voor de flatland revolt opgave voor de 
 - tweede vlaamse programmeerwedstrijd.
 -
 -}
module Main where

import Data.Char (isSpace)
import Data.List (break, partition, isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)
import System.IO

import Debug.Trace

type Angle = Float
type Distance = Float
type Position = Float
type Side = ((Position, Position), (Position, Position))

data Soldier = S { sx :: Int
                 , sy :: Int
                 , sr :: Float
                 , sh :: Int } deriving Show

data Bomb = B { bx :: Int
              , by :: Int
              , alpha :: Float
              , gamma :: Float
              , br :: Float } deriving (Eq, Ord, Show)

convertToRadians :: Int -> Float
convertToRadians angle = pi*(fromIntegral angle) / 180

splitOn :: String -> String -> [String]
splitOn sep ss = splitOn' ss sep []

splitOn' :: String -> String -> String -> [String]
splitOn' [] _ acc = [reverse acc]
splitOn' sss@(s:ss) sep acc = 
  case stripPrefix sep sss of
    Just suffix -> (reverse acc):splitOn' suffix sep []
    Nothing -> splitOn' ss sep (s:acc)


{- angles are expressed in degrees, so convert to radians -}
readBomb :: String -> Bomb
readBomb s = 
  let (x:y:alpha:gamma:r:_) = splitOn " " s
  in B (read x) (read y) (convertToRadians $ read alpha) (convertToRadians $ read gamma) (read r)

readSoldier :: String -> Soldier
readSoldier s = 
  let (x:y:r:h:_) = splitOn " " s
  in S (read x) (read y) ((read r :: Float) / 2) (read h)


polarise :: (Position, Position) -> (Distance, Angle)
polarise (x, y) 
  | x >= 0    = normalise $ (r, asin (y/r))
  | otherwise = normalise $ (r, pi - asin (y/r))
  where r = sqrt $ x*x + y*y
        normalise (r, angle) 
          | angle < 0 = (r, angle + 2*pi)
          | otherwise = (r, angle)

{-------------------------------------------------------------------------------------}
{- Is a single side destroyed, i.e., are two adjacent corners destroyed by the bombs -}
cornerDestroyed :: (Position, Position) -> Bomb -> Bool
cornerDestroyed (x, y) (B bx by ba bg br) =
  let x' = x - (fromIntegral bx)
      y' = y - (fromIntegral by)
      (pd, pa) = polarise (x', y')
  in pd <= br && (bg - ba/2) <= pa && (bg + ba/2) >= pa


computeCorners :: Soldier -> [(Position, Position)]
computeCorners (S sx sy sr sh) = 
  let alpha = 2*pi / (fromIntegral sh)
      angles = map (\i -> alpha * (fromIntegral i)) [0 .. (sh-1)]
      xs = map (\a -> (fromIntegral sx) + sr * (cos a)) angles
      ys = map (\a -> (fromIntegral sy) + sr * (sin a)) angles
  in zip xs ys


sideDestroyed :: Soldier -> [Bomb] -> Bool
sideDestroyed s bs = 
  let corners = computeCorners s
      destroyedCorners = map (\c -> or $  map (cornerDestroyed c) bs) corners
  in subsequence $ last destroyedCorners : destroyedCorners
  where subsequence (x:[]) = False
        subsequence (x:y:xs) = (x && y) || subsequence (y:xs)
{-------------------------------------------------------------------------------------}




{-------------------------------------------------------------------------------}
{- Is the soldier split into two parts, i.e., are one or more corners isolated -}

{- intersection of line segments through ordening of the endpoints -}
intersect :: Side -> Side -> Bool
intersect (a,b) (c,d) = (counterClockWise a c d) /= (counterClockWise b c d) && (counterClockWise a b c) /= (counterClockWise a b d) 
  where counterClockWise (xa, ya) (xb, yb) (xc, yc) = (yc - ya) * (xb - xa) > (yb - ya) * (xc - xa) 
  
soldierSides :: Soldier -> [Side]
soldierSides s = 
  let soldierCorners = computeCorners s
  in (last soldierCorners, head soldierCorners) : zip soldierCorners (tail soldierCorners)


bombSides :: Bomb -> [Side]
bombSides (B bx by ba bg br) =
  let bxf = fromIntegral bx
      byf = fromIntegral by
      bsTop = ((bxf, byf), (bxf + br * (cos (bg + ba/2)), byf + br * (sin (bg + ba/2))))
      bsBottom = ((bxf, byf), (bxf + br * (cos (bg - ba/2)), byf + br * (sin (bg - ba/2))))
  in [bsTop, bsBottom]


bombIntersectsWithBomb :: Bomb -> Bomb -> Bool
bombIntersectsWithBomb b1 b2 =
  let (b1s1:b1s2:_) = bombSides b1
      (b2s1:b2s2:_) = bombSides b2
  in or $ map (uncurry intersect) [(b1s1, b2s1), (b1s1, b2s2), (b1s2, b2s1), (b1s2, b2s2)]


{- A bomb intersects if it cuts a hole in a single side, if it would also contain the corner
 - in its radius, we would have processed it in the cornerDestroyed case -}
bombIntersectsWithSoldier :: Soldier -> Bomb -> Bool
bombIntersectsWithSoldier s b =
  let ss = soldierSides s
      (bs1:bs2:_) = bombSides b
  in or $ zipWith (&&) (map (intersect bs1) ss) (map (intersect bs2) ss)  


bombSplitsSoldier :: Soldier -> Bomb -> Bool
bombSplitsSoldier s b =
  let ss = soldierSides s
      (bs1:bs2:_) = bombSides b
  in ((==) 2 $ length $ filter (intersect bs1) ss) && ((==) 2 $ length $ filter (intersect bs2) ss)


soldierSplit :: Soldier -> [Bomb] -> Bool
soldierSplit s bs = or $ map (bombSplitsSoldier s) bs


soldierCornerCutOff :: Soldier -> [Bomb] -> Bool
soldierCornerCutOff s bs =
  let intersectingBombs = filter (uncurry bombIntersectsWithBomb) $ [ (b1, b2) | b1 <- bs, b2 <- bs, b1 < b2]
      potentialKillingBombs = filter (\(b1, b2) -> bombIntersectsWithSoldier s b1 && bombIntersectsWithSoldier s b2) intersectingBombs 
      soldierCorners = trace ("Corners = " ++ show (computeCorners s) ++ "\n" ++ "Bombs = " ++ show potentialKillingBombs) (computeCorners s)
      {- Check if we have both CCW and CW triangles for the soldiercorners -}
      cuttingBombs = map (\(b1, b2) -> let (b1c1, b1c2) = head $ bombSides b1
                                           (b2c1, b2c2) = head $ bombSides b2
                                           (b1CCW, b1CW) = partition (counterClockWise b1c1 b1c2) soldierCorners
                                           (b2CCW, b2CW) = partition (counterClockWise b2c1 b2c2) soldierCorners
                                       in trace (unlines $ [ show b1c1
                                                           , show b1c2
                                                           , show b2c1
                                                           , show b2c2
                                                           , show b1CCW
                                                           , show b1CW
                                                           , show b2CCW
                                                           , show b2CW] ) $ not $ or [null b1CCW, null b1CW, null b2CCW, null b2CW]) potentialKillingBombs
  in trace ("Cutting bombs = " ++ (show cuttingBombs)) (not $ null cuttingBombs)
  where counterClockWise (xa, ya) (xb, yb) (xc, yc) = (yc - ya) * (xb - xa) > (yb - ya) * (xc - xa)


{- determine kills for a given configuration, and return the unprocessed lines as well -}
processConfiguration :: [String] -> ([Bool], [String])
processConfiguration (h:lss) = 
  let (bombCount, soldierCount) = (\(x,y) -> (read x, read $ tail y)) $ break isSpace h :: (Int, Int)
      bombs = map readBomb $ take bombCount lss
      soldiers = map readSoldier $ take soldierCount $ drop bombCount lss

      dead1 = map (\(dead, soldier) -> dead || sideDestroyed soldier bombs) $ zip (repeat False) soldiers
      dead2 = map (\(dead, soldier) -> dead || soldierSplit soldier bombs) $ zip dead1 soldiers
      dead3 = map (\(dead, soldier) -> dead || soldierCornerCutOff soldier bombs) $ zip dead2 soldiers
  in (dead3, drop (bombCount + soldierCount) lss)



main = do
  cases <- read `fmap` hGetLine stdin :: IO Int
  lss <- lines `fmap` hGetContents stdin

  --putStrLn $ show cases
  --putStrLn $ unlines lss

  let kia = fst . last $ take cases $ drop 1 $ iterate (\(bss, ls) -> let (bs, ls') = processConfiguration ls in (bss ++ bs, ls')) ([], lss)
      output = map (\b -> if b then "dood" else "levend") kia

  putStr $ unlines output
