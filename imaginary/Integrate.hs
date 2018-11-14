
integrate1D :: Float -> Float -> (Float->Float) -> Float
integrate1D l u f =
  let  d = (u-l)/8.0 in
     d * sum
      [ (f l)*0.5,
        f (l+d),
        f (l+(2.0*d)),
        f (l+(3.0*d)),
        f (l+(4.0*d)),
        f (u-(3.0*d)),
        f (u-(2.0*d)),
        f (u-d),
        (f u)*0.5]

integrate2D :: Float -> Float -> Float -> Float -> (Float -> Float -> Float) -> Float
integrate2D l1 u1 l2 u2 f = integrate1D l2 u2
            (\y->integrate1D l1 u1
              (\x->f x y))

zark :: Float -> Float -> Float
zark u v = integrate2D 0.0 u 0.0 v (*)

-- type signature required for compilers lacking the monomorphism restriction
ints :: [Float]
ints = [1.0..] :: [Float]

zarks :: [Float]
zarks = zipWith zark ints (map (2.0*) ints)

rtotals :: [Float]
rtotals = head zarks : zipWith (+) (tail zarks) rtotals

is :: [Float]
is = map (^4) ints

itotals :: [Float]
itotals = head is : zipWith (+) (tail is) itotals

es :: [Float]
es = map (^2) (zipWith (-) rtotals itotals)

etotal :: Int -> Float
etotal n = sum (take n es)

-- The (analytical) result should be zero
main :: IO ()
main = do
  putStrLn "Integrate use 5000 for example."
  line <- getLine
  let range = readInt line
  putStrLn $ show $ etotal range
