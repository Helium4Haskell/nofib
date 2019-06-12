-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

main :: IO ()
main = do
    --putStrLn "Queens use 10 for example."
    line <- getLine
    if line == ""
     then return ()
     else do
        let nq = readInt line
        print $ nsoln nq
        main

nsoln :: Int -> Int
nsoln nq = length (gen nq)
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe _ _ []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: Int -> [[Int]]
    gen 0 = [[]]
    gen n = [ (q:b) | b <- gen (n-1), q <- [1..nq], safe q 1 b]
