main :: IO ()
main = do
    line <- getLine
    if line == ""
     then return ()
     else do
        let n = readInt line
        print n
        main
