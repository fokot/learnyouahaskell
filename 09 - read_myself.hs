import System.IO

{-
main = do
    handle <- openFile "read_myself.hs" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
-}

{-
main = do
    withFile "read_myself.hs" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
-}

main = do
    contents <- readFile "read_myself.hs"  
    putStr contents
