
module Shape where

    data Shape = Circle Float | Square Float | Rectangle Float Float | Triangle Float Float | Trapeze Float Float Float deriving (Show)

    area :: Shape -> Float
    area (Circle r) = pi * r * r
    area (Square s) = s * s
    area (Rectangle l w) = l * w
    area (Triangle b h) = 0.5 * b * h
    area (Trapeze b1 b2 h) = 0.5 * (b1 + b2) * h
    
    perimeter :: Shape -> Float
    perimeter (Circle r) = 2 * pi * r
    perimeter (Square s) = 4 * s
    perimeter (Rectangle l w) = 2 * (l + w)
    perimeter (Triangle b h) = b + 2 * (sqrt (h^2 + (b / 2)^2))
    perimeter (Trapeze b1 b2 h) = b1 + b2 + 2 * (sqrt (h^2 + ((b2 - b1) / 2)^2))
    
    instance Eq Shape where
        (==) s1 s2 = area s1 == area s2

    instance Ord Shape where
        (<=) s1 s2 = area s1 <= area s2
    

