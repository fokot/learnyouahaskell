import System.IO.Unsafe ( unsafePerformIO )

plus a b = unsafePerformIO( print ("calculating " ++ (show a) ++ " + " ++ (show b)) >> return (a + b))

f a b = a + b + a

main = do
    print "1 + 1 is calculated"
    print $ show (plus 1 1)
    print "1 + 1 is calculated again"
    print $ show (plus 1 1)
    print "1 + 2 is calculated"
    print $ show (plus 1 2)
    print "2 + 2 is calculated only once even though we have it twice in expression this is hot thunk works"
    print $ f (plus 2 2) 3
    print "2 + 2 is calculated again. this is another thunk (value that is yet to be evaluated) even though it has the same value as previous"
    print $ f (plus 2 2) 3