-- !!! the ultra-notorious "nfib 30" does w/ Floats
--

main :: IO ()
main = do
    --putStrLn "Nfib use 25 for example."
    line <- getLine
    if line == ""
     then return ()
     else do
        let n = readInt line
        print $ nfib n
        main

nfib :: Int -> Int
nfib n = if n <= 1 then 1 else nfib (n-1) + nfib (n-2) + 1

