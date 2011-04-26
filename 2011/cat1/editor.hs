module Main where

import Data.List (concat, concatMap, group, intersperse, splitAt)
import qualified Data.Map as M


makeEditMap :: [String] -> M.Map String Int
makeEditMap cs = foldl (\m s -> let (c:w:[]) = words s in M.insert w (read c) m) M.empty cs


makeCommandList :: [String] -> [(String, Int)]
makeCommandList cs = map (\s -> let (c:w:[]) = words s in (w, read c)) cs

edit :: String -> (String, Int) -> String
edit s (w, c) = 
    let gs = group $ words s
    in concat . intersperse " " $ map (\g -> concat . intersperse " " $ if w /= head g then g else replace g) gs
  where replace :: [String] -> [String]
        replace [] = []
        replace wss@(w':_) = if length wss >= c then let (rs, ts) = splitAt c wss in w : replace ts else wss 

main = do
    lss <- lines `fmap` getContents
    let cases = read $ lss !! 0 :: Int
  
    putStr . unlines . concatMap id . replaceLines $ tail lss

replaceLines :: [String] -> [[String]]
replaceLines [] = []
replaceLines lss@(sc:ls) = 
          let (_:sentences, cc:ls') = splitAt ((+) 1 $ read sc) lss
              (commands, ls'') = splitAt (read cc) ls'
              commandList = makeCommandList commands
          in map (\s -> foldl edit s commandList) sentences : replaceLines ls''


