-- JS Haskell tutorial written to Haskell by me
-- https://blog.jcoglan.com/2011/03/05/translation-from-haskell-to-javascript-of-selected-portions-of-the-best-introduction-to-monads-ive-ever-read
import Data.List

data DOMElement = DOMElement String [DOMElement] deriving (Show)

children :: DOMElement -> [DOMElement]
children (DOMElement _ k) = k

root :: DOMElement
root = DOMElement "root" [
            (DOMElement "uptown" [
                (DOMElement "downtown" []),
                (DOMElement "brooklyn" [])
            ]),
            (DOMElement "boss" [
                (DOMElement "worker 1" []),
                (DOMElement "worker 2" [])
            ])
        ]

--main = putStrLn $ show [root]


main = do
    putStrLn $ intercalate "\n" (map show grandchildren)
    where grandchildren = do
            a <- [root]
            b <- children a
            children b



