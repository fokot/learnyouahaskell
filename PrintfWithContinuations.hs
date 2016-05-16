-- tried things from talk Typed Routing with Continuations https://www.youtube.com/watch?v=tQI2JJwD_ZY

module PrintfWithContinuations where

c :: String -> ((String -> a) -> a)
c str = \k -> k str

d :: (String -> a) -> Int -> a
d = \k -> (\int -> k (show (int :: Int)))

s :: (String -> a) -> String -> a
s = \k -> (\str -> k str)

printf :: ((String -> IO ()) -> a) -> a
printf format = format putStrLn

(%) :: ((String -> a) -> c)
    -> ((String -> b) -> a)
    -> ((String -> b) -> c)
f1 % f2 = \k -> f1 (\s1 -> f2 (\s2 -> k (s1 ++ s2)))


formatted_1 = printf (c "aaa")

formatted_2 = printf d 12

formatted_3 = printf s "bbb"

formatted_4 = printf (c "This is " % s % c " " % d) "number" 3
