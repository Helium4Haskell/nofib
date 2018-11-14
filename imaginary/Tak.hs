
-- code of unknown provenance (partain 95/01/25)

tak :: Int -> Int -> Int -> Int
tak x y z = if not(y < x) then z
       else tak (tak (x-1) y z)
        (tak (y-1) z x)
        (tak (z-1) x y)

main :: IO ()
main = do
    putStrLn "Tak use 3 numbers [27 16 8] for example."
    line1 <- getLine
    let [word1,word2,word3] = words line1
        xs = readInt word1
        ys = readInt word2
        zs = readInt word3
    print (tak xs ys zs)
