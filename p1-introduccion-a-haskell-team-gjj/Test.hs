import Shape
import Point
import Haskellium
import List

import Data.List (sort)


mugiCheck :: Bool -> IO ()
mugiCheck a
  | a = putStrLn "Correcto! Kaizoku ou ni ore wa naru"
  | otherwise = putStrLn "Mal! Donificaste a Ace :C"

shapes :: [Shape]
shapes = [
            Circle 22.6,
            Square 22.6,
            Rectangle 2.26 5.0,
            Triangle 2.26 5.0
         ]

haskellium1 :: Haskellium
haskellium1 = Haskellium {
                name       = "Juanito",
                lastName1  = "Perez",
                lastName2  = "Lambda",
                location   = (100,2000),
                houseShape = shapes!!2
}
haskellium2 :: Haskellium
haskellium2 = Haskellium {
                name       = "Algo",
                lastName1  = "Ritmo",
                lastName2  = "C",
                location   = (40,61),
                houseShape = shapes!!0
}


----------------- SHAPES -----------------
-- PERIMETER
t1_perimeter :: Bool
t1_perimeter = perimeter (shapes!!0) == 142
t2_perimeter :: Bool
t2_perimeter=  perimeter (shapes!!2) == 14.52
--AREA
t1_area :: Bool
t1_area = area (shapes!!1) == 510.76000000000005
t2_area :: Bool
t2_area = area (shapes!!3) == 5.6499999999999995
-- ORD
t1_ord :: Bool
t1_ord = (sort shapes) == reverse shapes
----------------- POINT ---------------------
-- DISTANCE
t1_distance :: Bool
t1_distance = distance (1,1) (4,5) == 5
t2_distance :: Bool
t2_distance = distance (11,31) (42,5) == 40.459856648288
-- FROMO
t1_fromO :: Bool
t1_fromO = fromO (1,1) == 1.4142135623731
t2_fromO :: Bool
t2_fromO = fromO (11,31) == 32.893768406797
-------------- HASKELLIUMS -----------------
-- SON
t1_son :: Bool
t1_son = son haskellium1 haskellium2 "Mickey" == Haskellium {
                                                name       = "Mickey",
                                                lastName1  = "Perez",
                                                lastName2  = "Ritmo",
                                                location   = (100,2000),
                                                houseShape = shapes!!2
                                            }
t2_son :: Bool
t2_son = son haskellium2 haskellium1 "Mickey" == Haskellium {
                                                name       = "Mickey",
                                                lastName1  = "Ritmo",
                                                lastName2  = "Perez",
                                                location   = (40,61),
                                                houseShape = shapes!!0
                                                }
-- HOUSE COST
t1_houseCost :: Bool
t1_houseCost = houseCost haskellium1 == 58.9
t2_houseCost :: Bool
t2_houseCost = houseCost haskellium2 == 3564.2

-- TIME TO WORK
t1_timeToWork :: Bool
t1_timeToWork = timeToWork haskellium1 == 28.60712
t2_timeToWork :: Bool
t2_timeToWork = timeToWork haskellium2 == 2.4315062

-- LIST
t1_isPal :: Bool
t1_isPal = isPal (filter (\x -> x/=' ') "anita lava la tina")
t2_isPal :: Bool
t2_isPal = isPal (filter (\x -> x/=' ') "atar a la rata")

t1_concat :: Bool
t1_concat = concat' [[1,2,3],[4,5,6],[7,8,9]] == [1,2,3,4,5,6,7,8,9]
t2_concat :: Bool
t2_concat = concat' ["abcd", "efgh", "ijklm"] == "abcdefghijklm"

t1_pascal :: Bool
t1_pascal = pascalN 5 == [1,4,6,4,1]
t2_pascal :: Bool
t2_pascal = pascalN 14 == [1,13,78,286,715,1287,1716,1716,1287,715,286,78,13,1]

l1 = [1,2,3,4,5,6]; l2 = "esto es una lista de caracteres chalalala"
t1_reversaFr :: Bool
t1_reversaFr = reversaFr l1 == reverse l1
t2_reversaFr :: Bool
t2_reversaFr = reversaFr l2 == reverse l2

main = do
    putStrLn "Test Shapes"
    mugiCheck t1_perimeter
    mugiCheck t2_perimeter
    mugiCheck t1_area
    mugiCheck t2_area
    mugiCheck t1_ord 
    putStrLn ""
    putStrLn "Test Points"
    mugiCheck t1_distance
    mugiCheck t2_distance
    mugiCheck t1_fromO
    mugiCheck t2_fromO
    putStrLn ""
    putStrLn "Test Listas"
    mugiCheck t1_isPal
    mugiCheck t2_isPal
    mugiCheck t1_concat
    mugiCheck t2_concat
    mugiCheck t1_pascal
    mugiCheck t2_pascal
    mugiCheck t1_reversaFr
    mugiCheck t2_reversaFr
    putStrLn "Test Haskelliums"
    mugiCheck t1_timeToWork
    mugiCheck t2_timeToWork
    mugiCheck t1_son
    mugiCheck t2_son
    mugiCheck t1_houseCost
    mugiCheck t2_houseCost

    