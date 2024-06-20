module List where 

--Funcion que dada una cadena de texto nos dice si es palindromo o no
isPal :: String -> Bool
isPal palabra =
    palabra == reverse palabra

--Funcion que dada una lista de listas, nos devuelve la concatenacion
--de todas las listas contenidas en esta
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

--Funcion que regresa la n-esima fila del triangulo de pascal
pascalN :: Int -> [Int]
pascalN 1 = [1]
pascalN 2 = [1,1]
pascalN n = [1] ++ zipWith (+) (pascalN (n-1)) (tail (pascalN (n-1))) ++ [1]

--Funcion que dada una lista nos regresa la lista invertida
reversaFr :: [a] -> [a]
reversaFr [] = []
reversaFr lista = foldr (\x xs -> xs ++ [x]) [] lista
