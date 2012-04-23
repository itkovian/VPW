import Control.Applicative
import Data.List
import System.IO

data Direction = Omhoog | Omlaag | Evenwicht deriving (Eq, Show)
data Result = Zwaarder Char | Lichter Char | Onvoldoende | Tegenstrijdig deriving Eq

instance Show Result where
  show (Zwaarder c) = "Het valse geldstuk " ++ [c] ++ " is zwaarder."
  show (Lichter c) = "Het valse geldstuk " ++ [c] ++ " is lichter."
  show Onvoldoende = "Te weinig gegevens."
  show Tegenstrijdig = "Inconsistente gegevens."

mkWeigh :: [String] -> (String, String, Direction)
mkWeigh (left:right:d:[]) 
  | d == "omhoog" = (left, right, Omhoog)
  | d == "omlaag" = (right, left, Omhoog)
  | d == "evenwicht" = (left, right, Evenwicht)


collectAll :: [(String, String, Direction)] -> String
collectAll gs = 
  let ls = map (\(l, _, _) -> l) gs
      rs = map (\(_, r, _) -> r) gs
  in sort $ nub $ (concat ls) ++ (concat rs)


dropGenuines :: String -> (String, String, Direction) -> (String, String)
dropGenuines gs (left, right, _) = 
  let left' = left \\ gs
      right' = right \\ gs
  in (sort left', sort right')


guessLighter :: String -> String -> String
guessLighter ls rs = rs \\ ls

guessHeavier :: String -> String -> String
guessHeavier ls rs  = guessLighter rs ls


guess :: [(String, String, Direction)] -> Result
guess gs =
  let (es, us) = partition (\(_,_,d) -> d == Evenwicht) gs
      genuines = collectAll es
      us' = map (dropGenuines genuines) us 
      hss = sort $ nub $ concat $ map fst us'
      lss = sort $ nub $ concat $ map snd us'
      guessLs = guessLighter hss lss
      guessHs = guessHeavier hss lss
  in if null us then Onvoldoende 
     else if not . null $ hss `intersect` lss then Tegenstrijdig 
     else case (hss, lss) of 
      ([], []) -> Tegenstrijdig
      _ -> case guessHs `intersect` guessLs of
        (c:_) -> Tegenstrijdig
        [] -> case (guessHs, guessLs) of
              ([], []) -> Tegenstrijdig
              ([], c:[]) -> Lichter c
              (c:[], []) -> Zwaarder c
              ([a], [b]) -> Onvoldoende
              ([], _) -> Onvoldoende
              (_, []) -> Onvoldoende
              (a:b:_, c:[]) -> Lichter c
              (c:[], a:b:[]) -> Zwaarder c
              _ -> case (intersects $ map fst us', intersects $ map snd us') of
                      (a:_, _) -> Onvoldoende
                      (_, a:_) -> Onvoldoende
                      _ -> Tegenstrijdig
  where intersects = foldl1 intersect


process :: [String] -> [Result]
process [] = []
process (s:lss) = 
  let count = read s :: Int
      wss = map (mkWeigh . words) $ take count lss
  in guess wss : (process $ drop count lss)


main = do
  t <- read <$> getLine :: IO Int
  lss <- lines <$> getContents

  let checks = process lss
  putStr $ unlines $ map show checks

