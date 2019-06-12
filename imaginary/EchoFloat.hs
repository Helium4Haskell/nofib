main :: IO ()
main = do
    line <- getLine
    if line == ""
     then return ()
     else do
        let n = readFloat line
        print n
        main