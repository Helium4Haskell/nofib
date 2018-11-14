module Debug where

trace :: Show a => String -> a -> a
trace s x = unsafePerformIO $ do
    putStr $ "(" ++ s ++ "=" ++ show x ++ ")"
    return x
