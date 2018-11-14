module Ratio where

data Ratio = Ratio Int Int
    deriving (Eq,Show)

numerator :: Ratio -> Int
numerator (Ratio numer _) = numer

denominator :: Ratio -> Int
denominator (Ratio _ denom) = denom

reduce :: Int -> Int -> Ratio
reduce _ 0 = error "Ratio: divide by zero"
reduce x y = Ratio (x `quot` d) (y `quot` d)
    where
    d = gcd x y

ratio :: Int -> Int -> Ratio
ratio x y = reduce (x * signum y) (abs y)

negateRatio :: Ratio -> Ratio
negateRatio (Ratio x y) = Ratio (-x) y

plusRatio :: Ratio -> Ratio -> Ratio
plusRatio (Ratio x y) (Ratio x' y') = reduce (x*y' + x'*y) (y*y')

minusRatio :: Ratio -> Ratio -> Ratio
minusRatio (Ratio x y) (Ratio x' y') = reduce (x*y' - x'*y) (y*y')

sumRatio :: [Ratio] -> Ratio
sumRatio [] = Ratio 0 1
sumRatio (x:xs) = plusRatio x (sumRatio xs)
