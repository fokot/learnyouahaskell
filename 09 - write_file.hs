import System.IO
import Data.Char

main = do
    putStrLn $ "Give me filename"
    filename <- getLine
    putStrLn $ "Give me text to write there, I'll uppercase it"
    text <- getLine
    writeFile filename (map toUpper text)
    -- appendFile will just append to file if it exist
