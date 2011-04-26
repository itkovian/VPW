  -- | VPW opgave over lettergrepen
  -- thing

import Data.Array 
import Data.Char (isNumber, isSpace)
import Data.List
import Data.Map as M
import System.IO
import System.Environment
import Text.Regex.TDFA


type RegexMap = Map String (String -> [MatchArray])
type MatchMap = Map String [MatchOffset]


replaceBack :: String -> String
replaceBack [] = []
replaceBack s = 
  let s'@(h:ts) = reverse s
  in case h of 
    '.' -> reverse $ '$':ts
    _   -> s


replaceFront :: String -> String
replaceFront [] = []
replaceFront s@(h:ts) =
  case h of
    '.' -> '^':ts
    _   -> s


regexify :: String -> String
regexify s = 
  let s' = Prelude.filter (\c -> not (isNumber c || isSpace c)) s
  in replaceBack . replaceFront $ s'


replaceEqualsForbidden :: String -> String
replaceEqualsForbidden = Prelude.map (\c -> if c == '=' then '8' else c)

insertIntoMap :: RegexMap -> String -> RegexMap
insertIntoMap m s =
  let s' = replaceEqualsForbidden s
      s'' = regexify s
  in M.insert s' (\string -> string =~ s'' :: [MatchArray]) m


-- | Find the rules that apply to the given string and return the indices
-- of the positions in the string where they apply. Of course, the rule
-- itself is also returned
--
getApplicableRules :: RegexMap -> String -> MatchMap 
getApplicableRules m s = M.mapMaybe (\rule -> let offsets =  Prelude.map fst . concat . Prelude.map Data.Array.elems $ rule s :: [MatchOffset]
                                              in case offsets of 
                                                 [] -> Nothing
                                                 _  -> Just offsets) m 

-- | Find the spots in which to drop the integer priorities
-- for any rule that matches the target word
--
positionPriority :: String -- ^ rule
                 -> Int    -- ^ matching position
                 -> [(Int, Int)] -- ^ tuple with the priority and the matching position
positionPriority rule loc =
    let ls = asp rule loc
        ps = Data.List.map (read . \c -> [c]) $ Prelude.filter isNumber rule
    in zip ls ps
  where asp (c:cs) i 
          | isNumber c = i:asp cs i
          | c == '.' = asp cs i 
          | otherwise = asp cs (i+1) 
        asp [] _ = []


-- | Do the actual hypenantion
--
hyphenate :: RegexMap -> String -> String
hyphenate regexMap s@(c:cs)
  | length s < 5 = s
  | otherwise = 
    let matchMap = getApplicableRules regexMap s
        positions = Prelude.map fst                                                                   -- retain indices
                  . reverse                                                                           -- resort by position
                  . Prelude.filter (\(_, r) -> r `mod` 2 /= 0)                                        -- retain the rules that imply a hyphen should be put
                  . Prelude.map head                                                                  -- the first of the group has the highest priority
                  . groupBy (\(p1, _) (p2, _) -> p1 == p2)                                            -- group by position
                  . sortBy (\p1 p2 -> compare p2 p1)                                                  -- sort in descending order. this puts the highest priority first
                  . Prelude.concat                                                                    -- flatten
                  . M.elems                                                                           -- get the (k,v) tuples from the map
                  . M.mapWithKey (\k vs -> Prelude.concat $ Prelude.map (positionPriority k) vs)      -- transform the map to get the positions and priorities
                  $ matchMap
    in c : hyp cs 1 positions
  where hyp :: String -> Int -> [Int] -> String
        hyp [] _ _ = []
        hyp cs _ [] = cs
        hyp (c:cs) j (i:is) 
          | i < j  = hyp (c:cs) j is 
          | i == j = '-':c: hyp cs (j+1) is
          | otherwise = c : hyp cs (j+1) (i:is)


-- | Is this string an exception?
--
isException :: [String] -> String -> (String, Bool)
isException [] s = (s, False)
isException (e:es) s = 
    let e' = Prelude.filter ((/=) '-') e
    in if e' == s then (e, True) else isException es s

main = do 
  lss <- lines `fmap` getContents
  let rules = takeWhile (not . isPrefixOf "----------" ) lss
      --exceptions = takeWhile (not . isPrefixOf "----------") . tail . dropWhile (not . isPrefixOf "----------") $ lss
      --ws = Data.List.filter (not . Data.List.null) . tail . dropWhile (not . isPrefixOf "----------") . tail . dropWhile (not . isPrefixOf "----------") $ lss
      ws = Data.List.filter (not . Data.List.null) . tail . dropWhile (not . isPrefixOf "----------") $ lss
      regexMap = foldl' (\m s -> insertIntoMap m s) M.empty rules

      --hyphs = Prelude.map (\s -> let (e, b) = isException exceptions s in if b then e else hyphenate regexMap s) ws
      hyphs = Prelude.map (hyphenate regexMap) ws

  putStr $ unlines hyphs

  --putStrLn $ show $ getApplicableRules regexMap "mutsje"
  --putStrLn $ show $ getApplicableRules regexMap "spiegelei"
  --putStrLn $ show $ getApplicableRules regexMap "egelei"
  --putStrLn $ show $ getApplicableRules regexMap "egel"


