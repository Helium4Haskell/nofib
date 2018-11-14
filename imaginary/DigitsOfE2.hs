carryPropagate :: Int -> [Int] -> [Int]
carryPropagate base (d:ds)
  | carryguess == (d+9) `div` base
      = carryguess : (remainder+nextcarry) : fraction
  | otherwise
      = (dCorrected `div` base) : (dCorrected `mod` base) : fraction
  where
    carryguess = d `div` base
    remainder = d `mod` base
    --nextcarry:fraction = carryPropagate (base+1) ds
    carryProp = carryPropagate (base+1) ds
    nextcarry = head carryProp
    fraction = tail carryProp
    dCorrected = d + nextcarry

e :: String
e = ("2."++) $
    tail . concat $
    map (show.head) $
    iterate (carryPropagate 2 . map (10*) . tail) $
    2:[1,1..]

main :: IO ()
main = do
    putStrLn "DigitsOfE2 use 10 for example."
    line <- getLine
    let n = readInt line
    print (take n e)
