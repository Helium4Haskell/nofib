-- !!! the ultra-notorious "nfib 30" does w/ Floats
--

main :: IO ()
main = do
    putStrLn "Nfib use 25 for example."
    line <- getLine
    let n = intToFloat $ readInt line
    print $ nfib n

nfib :: Float -> Float
nfib n = if n <= 1.0 then 1.0 else nfib (n-1.0) + nfib (n-2.0) + 1.0

