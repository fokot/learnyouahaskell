import Control.Applicative

concatAction :: IO String  
concatAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b

--concatAction = concatAction2
concatAction2 :: IO String
concatAction2 = (++) <$> getLine <*> getLine