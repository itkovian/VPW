import System.IO
import System.Environment
import Data.Char (ord, chr, isLower, isUpper, isSpace)

upperA = ord 'A'

encode :: (Char -> Char -> Char) -> String -> String
encode coder ls = 
  let key = head . words $ ls
      text = tail . dropWhile ((/=) ' ') $ ls
  in zipWith coder text (concat $ repeat key)

encode' = encode'' (+)
decode' = encode'' (-)

encode'' :: (Int -> Int -> Int) -> Char -> Char -> Char
encode'' op c offset = 
  let targetVal = (val c `op` val offset) `mod` 27
  in case targetVal of
    0 -> ' '
    _ -> chr $ upperA + targetVal - 1
  where val x
          | isSpace x = 0
          | otherwise = ord x - upperA + 1

main = do
  lss <- lines `fmap` getContents
  let numberOfUncodedLines = read . head $ lss :: Int
      uncodedLines = take numberOfUncodedLines $ tail lss
      codedLines = tail . drop numberOfUncodedLines . tail $ lss -- we need not see the number, they are the remaining lines until EOF is reached

      cs = map (encode encode') uncodedLines
      ucs = map (encode decode') codedLines

  putStr $ unlines (cs ++ ucs)
