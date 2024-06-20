module Haskellium where
    import Point
    import Shape
    import GHC.Weak (deRefWeak)
    
    data Haskellium = Haskellium { 
                  name       :: String,
                  lastName1  :: String, 
                  lastName2  :: String, 
                  location   :: Punto,
                  houseShape :: Shape
    } deriving (Show)


    --Funcion que dado dos haskelliums y un String de nombre, regresa un hijo Haskellium
    --con el nombre dado y los apellidos de los padres
    son :: Haskellium -> Haskellium -> String -> Haskellium
    son haskelliumP haskelliumM name = Haskellium {
                name       = name,
                lastName1  = lastName1 haskelliumP,
                lastName2  = lastName1 haskelliumM,
                location   = location haskelliumP,
                houseShape = houseShape haskelliumP
    }

    
    

     
    --Funcion que dado un Haskellium, calcula el tiempo en unidades t, el tiempo que le toma llegar a su trabajo
    --Si la distancia de la localizacion del haskellium al centro de la isla es menor a 300, el haskellium se va en bicicleta 
    --y llegara al trabajo moviendose 30u/t.
    --En otro caso, se va en motocicleta y llegara al trabajo moviendose 70u/t
    timeToWork :: Haskellium -> Float
    timeToWork haskellium = case fromO(location haskellium) of
                                x | x < 300 -> (fromO(location haskellium)) / 30 
                                x | x > 300 -> (fromO(location haskellium)) / 70 
                             


    --instancia para comparar dos haskelliums
    instance Eq Haskellium where 
        (==) haskellium1 haskellium2 = (name haskellium1 == name haskellium2) && (lastName1 haskellium1 == lastName1 haskellium2) && (lastName2 haskellium1 == lastName2 haskellium2) && (location haskellium1 == location haskellium2) && (houseShape haskellium1 == houseShape haskellium2)


    ----------------- HASKELLIUM de prueba -----------------
    haskelliumPrueba :: Haskellium
    haskelliumPrueba = Haskellium {
                name       = "Juanito",
                lastName1  = "Perez",
                lastName2  = "Lambda",
                location   = (55,77),
                houseShape = Rectangle 2.6 5.0
    }