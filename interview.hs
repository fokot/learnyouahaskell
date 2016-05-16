-- Solution will run in Haskell Interpreted Mode
-- done in https://coderpad.io

import Data.Char hiding (digitToInt)

-- main = putStrLn "Hello, World!"

inputs = ["123", "123.xx", "123.9", "123-", "-123-", "   -123", "1a", "a" ," 1 23"]

-- main  = mapM (\i -> putStrLn $ i ++ " = " ++ show (to_i i)) inputs

main = putStrLn $ show (ord '2')

{- 
Your previous Plain Text content is preserved below:

to_i('123') #=> 123
to_i('123.xx') #=> 123
to_i('123.9') #=> 123
to_i('123-') #=> 123
to_i('-123-') #=> -123
to_i('   -123') #=> -123
to_i(' 1 23') #=> 1
to_i('1a') #=> 1
to_i('a') #=> 0
 -}

-- O(n)

to_i :: String -> Int
to_i [] = 0
to_i ('-':xs) = - (to_i xs)
to_i (' ':xs) = to_i xs
to_i xs = to_i' xs 0
  where to_i' [] acc = acc
        to_i' (x:xs) acc = maybe acc (\d -> to_i' xs (acc * 10 + d)) (digitToInt x)
            
digitToInt :: Char -> Maybe Int
digitToInt '1' = Just 1
digitToInt '2' = Just 2
digitToInt '3' = Just 3
digitToInt '4' = Just 4
digitToInt '5' = Just 5
digitToInt '6' = Just 6
digitToInt '7' = Just 7
digitToInt '8' = Just 8
digitToInt '9' = Just 9
digitToInt '0' = Just 0
digitToInt  _  = Nothing


-- '0' => 48
-- '1' => 49
-- codepoint - 48

