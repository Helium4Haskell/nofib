
suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter [] = []
the_filter (n:ns) = filter (isdivs n) ns

primes :: [Int]
primes = map head (iterate the_filter (iterate suCC 2))

main :: IO ()
main = do
    --putStrLn "Primes use 1500 for example."
    line <- getLine
    if line == ""
     then return ()
     else do
        let n = 100 * readInt line
        print $ primes !! n
        main
