module Sat where

import Prop

type Literal = Prop
type Clausula = [Literal]

--Funcion que dada una formula proposicional en forma FNC devuelve 
--una lista con las clausulas de las cuales se conforma. 
clausulas :: Prop -> [Clausula]
clausulas s = case s of 
                Var x -> [[Var x]]
                Not (Var x) -> [[Not (Var x)]]
                Or p q -> [clausulas p !! 0 ++ clausulas q !! 0]
                And p q -> clausulas p ++ clausulas q
   
{-Funcion que dadas dos clausulas devuelve el
resolvente obtenido de aplicar resolucion binaria. Se asume que
se puede obtenener un resolvente-} 
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 = [x | x <- c1, not (esNegacion x c2)]
    where   esNegacion (Not x) c2 = x `elem` c2
            esNegacion x c2 = Not x `elem` c2

-- Determina si existe una resolvente a partir de dos clausulas
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = or [esNegacion x y | x <- c1, y <- c2]
    where esNegacion (Not x) y = x == y
          esNegacion x (Not y) = x == y
          esNegacion x y = False

--Funcion que dada una formula proposicional y el algoritmo de saturacion
--determina si es satisfacible o no.  
saturacion :: Prop -> Bool
saturacion formulaP = sat (clausulas (fnc formulaP))

{-Funcion auxiliar para saturacion. Dada una lista de clausulas realiza de
forma recursiva el algoritmo de saturacion. 
Devuelve False si hemos encontrado la clausula vacia 
Devolvemos True si ya no podemos generar nuevos resolventes 
En caso contrario, seguimos iterando-}
sat :: [Clausula] -> Bool
sat clausulas
         | [] `elem` clausulas = False 
         | otherwise = 
             let resolventes = [(resolucion c1 c2) | c1 <- clausulas, c2 <- clausulas, hayResolvente c1 c2]
                 nuevasclausulas = elemRepetidos (clausulas ++ resolventes)
             in 
                if length nuevasclausulas == length clausulas then True 
                else sat nuevasclausulas

--Funcion aux para sat. Devuelve la misma lista dada sin elementos repetidos.  
elemRepetidos :: Eq a => [a] -> [a]
elemRepetidos [] = []
elemRepetidos (x:xs) = x : elemRepetidos (filter(/= x) xs)