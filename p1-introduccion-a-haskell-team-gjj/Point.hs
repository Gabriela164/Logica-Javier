module Point where
    
    type Punto = (Float, Float)

    distance :: Punto -> Punto -> Float
    distance (x,y)(a,b) = sqrt((a - x)*(a - x) + (b - y)*(b - y) )

    fromO :: Punto -> Float
    fromO (x,y) = sqrt((x * x) + (y * y)) 