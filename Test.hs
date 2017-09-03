module Test where

type Hours = Integer
type Minutes = Integer
data Time = Time Hours Minutes deriving (Eq, Show)


instance Num Time where
    (Time h m) + (Time h' m') = let sm = m + m'
                                    h'' = quot sm 60
                                    m'' = rem sm 60
                                in Time (h + h' + h'') m''
    (Time h m) - (Time h' m') = let sb = m - m'
                                    h'' = quot sb 60
                                    m'' = rem sb 60
                                in Time (h - h' - h'') m''
    fromInteger x = let m = rem x 60
                        h = quot x 60
                    in Time h m
    abs (Time h m) = Time (abs h) (abs m)
    (Time h m) * x = (Time h m)
    signum a = a


(*) :: Time -> Int -> Time
(Time h m) * x = (Time h m + 4)