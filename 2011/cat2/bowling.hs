-- | Solution to the bowling score problem - VPW2011 Een spelletje bowling
-- wordt gespeeld over tien frames en heeft een score- verloop zoals wordt
-- ge ̈ıllustreerd in bovenstaande figuur. Binnen elk frame heeft een speler
-- twee mogelijkheden om tien kegels omver te werpen. De score voor een frame
-- is het totaal aantal omvergeworpen kegels, plus een extra bonus voor strikes
-- en spares.  Wanneer een speler alle tien de kegels kan omverwerpen in twee
-- beurten, dan spreken we van een spare. De bonus voor dat frame is het aantal
-- kegels dat wordt omvergeworpen bij de eerstvolgende worp. In frame 3 van het
-- voorbeeldspelletje dat hierboven staat weergegeven bedraagt de score dus 10
-- (het totaal aantal omver geworpen kegels) plus een bonus van 5 (het aantal
-- omver geworpen kegels bij de volgende worp).  Wanneer een speler bij de
-- eerste worp van een frame alle kegels kan om- verwerpen, dan spreken we van
-- een strike. Bij een strike blijft het aantal worpen in het frame beperkt tot
--  ́e ́en. De bonus voor dat frame is het totaal aantal kegels dat omvergeworpen
--  wordt bij de volgende twee worpen. Indien een speler in het tiende frame
--  een spare of strike werpt, dan mag hij extra ballen blijven werpen om het
--  frame af te werken. Er mogen echter nooit meer dan drie ballen geworpen
--  worden in het tiende frame. In het tiende frame worden verder geen
--  bonuspunten toegekend.
--

module Main where

import Data.List (concat, intersperse)

isStrike :: Int -> Bool
isStrike n = n == 10

isSpare :: [Int] -> Bool
isSpare (p:p':_) = p + p' == 10
isSpare _ = False

score :: [Int] -> Int -> [Int] -> Maybe [Int]
score [] c acc
  | c == 10 = Just $ reverse acc
  | otherwise = Nothing
score pss@(p:ps) c acc
  | c >= 10 = Nothing
  | c == 9 = lastFrame pss acc
  | otherwise = normalFrame pss c acc

lastFrame :: [Int] -> [Int] -> Maybe [Int]
lastFrame (p:ps) acc
  | isStrike p || isSpare (p:ps) = case ps of
                                      p1:p2:[] -> Just $ reverse (p+p1+p2:acc)
                                      _ -> Nothing
  | otherwise = case ps of
                  p1:[] -> Just $ reverse (p+p1:acc)
                  _ -> Nothing

normalFrame :: [Int] -> Int -> [Int] -> Maybe [Int]
normalFrame (p:ps) c acc 
  | isStrike p = case ps of 
                    p1:p2:_ -> let current = p + p1 + p2 in score ps (c+1) (current:acc) 
                    _ -> Nothing
  | isSpare (p:ps) = case ps of 
                        p1:p2:_ -> let current = p + p1 + p2 in score (tail ps) (c+1) (current:acc)
                        _ -> Nothing
  | otherwise = case ps of
                  p1:_ -> let current = p + p1 in if current > 10 then Nothing else score (tail ps) (c+1) (current:acc)
                  [] -> Nothing

main = do
  lss <- lines `fmap` getContents

  let count = read . head $ lss :: Int
      cases = map (map read . words) $ tail lss :: [[Int]]
      scores = map (\ps -> score ps 0 []) cases
      frames = map (\ss -> case ss of 
                              Just s -> Just $ scanl1 (+) s
                              Nothing -> Nothing) scores

  putStr $ unlines . map (\frame -> case frame of
                                        Just f -> concat . intersperse " " . map show $ f
                                        Nothing -> "ONGELDIG") $ frames
