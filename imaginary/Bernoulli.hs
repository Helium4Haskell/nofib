-- There was a lot of discussion about various ways of computing
-- Bernouilli numbers (whatever they are) on haskell-cafe in March 2003
-- Here's one of the programs.

-- It's not a very good test, I suspect, because it manipulates big integers,
-- and so probably spends most of its time in GMP.

{- Ratio-}
import Ratio
import Debug
{- Benchmark -}

-- powers = [[r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
powers :: [[Int]]
powers = [2..] : map (zipWith (*) (head powers)) powers

-- powers = [[(-1)^r * r^n | r<-[2..]] | n<-1..]
-- type signature required for compilers lacking the monomorphism restriction
neg_powers :: [[Int]]
neg_powers = map (zipWith (\n x -> if n then x else -x) (iterate not True)) powers

pascal:: [[Int]]
pascal = [1,2,1] : map (\line -> zipWith (+) (line++[0]) (0:line)) pascal

bernoulli :: Int -> Ratio
bernoulli 0 = ratio 1 1
bernoulli 1 = negateRatio (ratio 1 2)
bernoulli n
    | odd n = ratio 0 1
    | otherwise = plusRatio (ratio (-1) 2) (sumRatio
        [ ratio ((sum $ zipWith (*) powers' (tail $ tail combs)) - k) (k+1)
        | (k,combs) <- zip [2..n] pascal])
    where powers' = (neg_powers !! (n-1))

main :: IO ()
main = do
    {-putStrLn "Bernoulli use 15 as max."
    line <- getLine
    let n = readInt line
    putStrLn $ "Bernoulli " ++ (show n) ++ " : " ++ show (bernoulli n)-}
    --putStrLn $ "Bernoulli table until: " ++ (show n)
    --putStr $ unlines $ map (\x -> show x ++ " : " ++ show (bernoulli x)) [0..n]
    line <- getLine
    if line == ""
     then return ()
     else do
        let n = readInt line
        print $ bernoulli n
        main
