{-# LANGUAGE FlexibleInstances #-}
import Control.Monad
import Data.Char
import Data.List
import System.IO


data Blood = A | B | O deriving (Eq, Ord, Show)
data Rhesus = N | P deriving (Eq, Ord, Show)

data BT = BT ([(Blood, Blood)], [(Rhesus, Rhesus)]) | Unknown deriving (Eq, Ord, Show)

type RBT = ([Blood], Rhesus)


mkBlood :: Char -> Blood
mkBlood c
  | c == 'A' = A
  | c == 'B' = B
  | c == 'O' = O


mkRhesus :: Char -> Rhesus
mkRhesus c
  | c == '+' = P
  | c == '-' = N


expandBlood :: [Blood] -> [(Blood, Blood)]
expandBlood (O:[]) = [(O,O)]
expandBlood (b:[]) = [(b,b), (b,O)]
expandBlood (b:b':[]) = [(b, b')]

expandRhesus :: Rhesus -> [(Rhesus, Rhesus)]
expandRhesus P = [(P,P), (N,P)]
expandRhesus N = [(N,N)]

parseBT :: String -> BT
parseBT bs = 
  let b = takeWhile isAlpha bs
      r = head $ dropWhile isAlpha bs
  in case r of
    '?' -> Unknown
    _   -> BT (expandBlood $ map mkBlood b, expandRhesus $ mkRhesus r) 

mkCase :: String -> (BT, BT, BT)
mkCase s = 
  let (m:d:k:_) = words s
  in (parseBT m, parseBT d, parseBT k) 

orderBT :: (Blood, Blood) -> (Blood, Blood)
orderBT (a, b)
  | a <= b    = (a,b)
  | otherwise = (b, a)

orderRH :: (Rhesus, Rhesus) -> (Rhesus, Rhesus) 
orderRH (a, b)
  | a <= b    = (a,b)
  | otherwise = (b,a)

combineBT :: (Blood, Blood) -> (Blood, Blood) -> [(Blood, Blood)]
combineBT (ba1, ba2) (bb1, bb2) = nub $ map orderBT $ [(ba1, bb1), (ba1, bb2), (ba2, bb1), (ba2, bb2)]

combineRH :: (Rhesus, Rhesus) -> (Rhesus, Rhesus) -> [(Rhesus, Rhesus)]
combineRH (ra1, ra2) (rb1, rb2) = nub $ map orderRH $ [(ra1, rb1), (ra1, rb2), (ra2, rb1), (ra2, rb2)]
      
determineChild :: BT -> BT -> [BT]
determineChild (BT (mbs, mrs)) (BT (pbs, prs)) = 
  let cbs = nub $ concat $ [ combineBT mb pb | mb <- mbs, pb <- pbs]
      crs = nub $ concat $ [ combineRH mr pr | mr <- mrs, pr <- prs]
  in [ BT ([cb], [cr]) | cb <- cbs, cr <- crs]


canHaveChild :: BT -> BT -> BT -> Bool
canHaveChild p@(BT (pbs, prs)) c@(BT (cbs, crs)) m@(BT (mbs, mrs)) =
  let cbs' = nub $ map orderBT $ concat $ [ combineBT pb mb | pb <- pbs, mb <- mbs ]
      crs' = nub $ map orderRH $ concat $ [ combineRH pr mr | pr <- prs, mr <- mrs ]
  in (not . null $ intersect cbs' cbs) && (not . null $ intersect crs' crs)


determineParent :: BT -> BT -> [BT]
determineParent p c = 
  let allBT = [(A, A), (A, B), (A, O), (B, B), (B, O), (O, O)]
      allRH = [(P,P), (P, N), (N, N)]
      allCombinations = [ BT ([b], [r]) | b <- allBT, r <- allRH]
  in filter (canHaveChild p c) allCombinations


determineMissing :: (BT, BT, BT) -> ([BT], [BT], [BT])
determineMissing (m, p, Unknown) = ([m], [p], determineChild m p)
determineMissing (Unknown, p, c) = (determineParent p c, [p], [c])
determineMissing (m, Unknown, c) = ([m], determineParent m c, [c])


reduceBT :: (Blood, Blood) -> [Blood]
reduceBT (v, O) = [v]
reduceBT (O, v) = [v]
reduceBT (u, v)
  | u == v = [u]
  | otherwise = sort [u, v]


reduceRH :: (Rhesus, Rhesus) -> Rhesus
reduceRH (P, _) = P
reduceRH (_, P) = P
reduceRH (N, N) = N


reduce :: BT -> [([Blood], Rhesus)]
reduce (BT (bs, rs)) = 
  let bs' = map reduceBT bs
      rs' = map reduceRH rs
  in nub [ (b, r) | b <- bs', r <- rs' ]


printBT :: RBT -> String
printBT (bs, r) = 
  let rS = case r of
            P -> "+"
            N -> "-"
  in (concat $ map show bs) ++ rS


printBTs :: [RBT] -> String
printBTs [] = "ONMOGELIJK"
printBTs (b:[]) = printBT b
printBTs bs = "{" ++ (concat . intersperse "," $ sort $ map printBT bs) ++ "}"


prettyPrint :: ([RBT], [RBT], [RBT]) -> String
prettyPrint (p, m, c) = concat . intersperse " " $ map printBTs [p, m, c]


main = do
  t <- read `liftM` getLine :: IO Int
  lss <- lines `liftM` getContents

  let cases = map mkCase lss
      bss = map determineMissing cases
      rss = map (\(ms, ps, cs) -> (nub $ concat $ map reduce ms, nub $ concat $ map reduce ps, nub $ concat $ map reduce cs)) bss
      sss = map prettyPrint rss
  
  putStr $ unlines sss
  
