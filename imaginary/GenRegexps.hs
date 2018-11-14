-- !!! Wentworth's version of a program to generate
-- !!! all the expansions of a generalised regular expression
-- !!!
--
-- RJE: Modified so it only outputs the number of characters in the output,
-- rather that the output itself, thus avoiding having to generate such a
-- huge output file to get a reasonable execution time.

import Char

main :: IO ()
main = do
    putStrLn "GenRegexps use \"[a-j][a-j][a-j]abcdefghijklmnopqrstuvwxyz\" for example.\nResults in => Enter a generator: 29000"
    line <- getLine
    print $ (("Enter a generator: " ++) . show . numchars . expand) line

numchars :: [String] -> Int
numchars l = sum $ map length l

expand :: String -> [String]
expand []       = [""]
expand ('<':x)  = numericRule x
expand ('[':x)  = alphabeticRule x
expand x        = constantRule x

constantRule :: String -> [String]
constantRule (c:rest)   = [ c:z | z <- expand rest ]
constantRule _ = error "constantRule"

alphabeticRule :: String -> [String]
alphabeticRule (a:'-':b:']':rest)
  | a <= b      = [c:z | c <- [a..b],         z <- expand rest]
  | otherwise   = [c:z | c <- reverse [b..a], z <- expand rest]
alphabeticRule _ = error "alphabeticRule"

numericRule :: String -> [String]
numericRule x
  = [ pad (show i) ++ z
  | i <- if u < v then [u..v] else [u,u-1..v]
  , z <- expand (tail ss) ]
  where
    (p,qs) = span (/= '-') x
    (r,ss) = span (/= '>') (tail qs)
    (u,v)   = (mknum p, mknum r)
    mknum w = foldl (\ t c -> t * 10 + (ord c - ord '0')) 0 w
    pad z   = [ '0' | _ <- [1 .. (width-(length z))]] ++ z
    width   = max (length (show u)) (length (show v))
