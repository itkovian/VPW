{-
 - (C) 2012 Andy Georges
 -
 - Oplossing voor de VPW 2012 opgave gevangen.
 -
 -}


module Main where

data Piece = W
           | L 
           | Z
           deriving (Eq)

swap :: Piece -> Piece
swap p = case p of
            W -> Z
            Z -> W
            _ -> L

mkPiece :: Char -> Piece
mkPiece c = 
    case c of
        'W' -> W
        'L' -> L
        'Z' -> Z
        _ -> error "Do I know you?"

shiftAndJump :: (Int, Int) -> [Piece] -> (Int, Int)
shiftAndJump (j, s) [] = (j, s)
shiftAndJump (j, s) (p:ps)
  | p /= W    = shiftAndJump (j, s) ps
  | otherwise = case ps of
                  L:ps'     -> shiftAndJump (j, s+1) ps'
                  Z:L:ps'   -> shiftAndJump (j+1, s) ps'
                  Z:ps'     -> shiftAndJump (j, s) ps'
                  _         -> shiftAndJump (j, s) ps


solve :: IO String
solve = do
    ((p:_):bs) <- (map (map mkPiece) . words) `fmap` getLine
    let (j, s) = shiftAndJump (0,0) $ case p of 
                                        Z -> reverse . map swap $ concat bs
                                        W -> concat bs
    return $ show j ++ " " ++ show s

main = do
    n <- read `fmap` getLine
    s <- sequence $ replicate n solve
    mapM putStrLn s

