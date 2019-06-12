
type ContFrac = [Int]

eContFrac :: ContFrac
eContFrac = 2:aux 2 where aux n = 1:n:1:aux (n+2)

ratTrans :: (Int,Int,Int,Int) -> ContFrac -> ContFrac
ratTrans (a,b,c,d) xs |
  ((signum c == signum d) || (abs c < abs d)) &&
  (c+d)*q <= a+b && (c+d)*q + (c+d) > a+b
     = q:ratTrans (c,d,a-q*c,b-q*d) xs
  where q = b `div` d
ratTrans (a,b,c,d) (x:xs) = ratTrans (b,a+x*b,d,c+x*d) xs

toDigits :: ContFrac -> [Int]
toDigits (x:xs) = x:toDigits (ratTrans (10,0,0,1) xs)

e :: [Int]
e = toDigits eContFrac

main :: IO ()
main = do
    --putStrLn "DigitsOfE1 use 10 for example."
    line <- getLine
    if line == ""
     then return ()
     else do
        let n = readInt line
        print (take n e)
        main
