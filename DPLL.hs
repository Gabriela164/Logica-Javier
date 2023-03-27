
module DPLL where

import Data.List
import Prop

type Interpretacion = [(String, Bool)]

type Estado = (Interpretacion, [Clausula])

-- Funcion que aplica la regla conflict. Determina si la busqueda del modelo falla, es decir,
-- si la clausula vacia [] forma parte del conjunto de clausulas.
conflict :: Estado -> Bool
conflict (interp, clausulas) = [] `elem` clausulas

-- Funcion que aplica la regla success. Determina si la busqueda del modelo ha sido exitosa.
-- Devolvemos True si el conjunto de clausulas es vacio, en caso contrario False.
success :: Estado -> Bool
success (inter, clausulas) = null clausulas

-- Funcion que aplica la regla Unit. Aplicamos esta regla solo a la primer literal que se encuentre.
--Si no se puede aplicar la rregla unit concatenamos a las clausulas la clausula vacia []
unit :: Estado -> Estado
unit estado = case encuentraLit (snd estado) of
  Nothing -> estado
  Just l ->
    if contieneComplementaria l (fst estado)
      then estado
      else case l of
        (V s) -> (fst estado ++ [(s, True)], elimLit l (snd estado))
        (NotV s) -> (fst estado ++ [(s, False)], elimLit l (snd estado))

-- Funcion auxiliar para unit. Determina si el complemento de una literal se encuentra en interp
contieneComplementaria :: Literal -> Interpretacion -> Bool
contieneComplementaria l interp = case l of
  (V s) -> (s, False) `elem` interp
  (NotV s) -> (s, True) `elem` interp

-- Funcion auxiliar para unit. Encuentra la primer literal unitaria en el conjunto de clausulas
encuentraLit :: [Clausula] -> Maybe Literal
encuentraLit = foldr (\clausula acc -> if length clausula == 1 then Just (head clausula) else acc) Nothing

-- Funcion auxiliar para unit. Elimina la literal unitaria del conjunto de clausulas
elimLit :: Literal -> [Clausula] -> [Clausula]
elimLit l = filter (\clausula -> not (length clausula == 1 && head clausula == l))

-- Funcion que aplica la regla Eliminacion.
elim :: Estado -> Estado
elim (interp, clausulas) = (interp, [cl | cl <- clausulas, not (any (\(s, b) -> V s `elem` cl && b || NotV s `elem` cl && not b) interp)])

-- Funcion que aplica la regla de reduccion. Dicha regla la aplica para cada literal que haya en la interpretacion del estado.
red :: Estado -> Estado
red (interp, clausulas) = case interp of
  [] -> (interp, clausulas)
  -- Si la interpretacion no es vacia, aplicamos la regla de reduccion para cada literal de la interpretacion.
  _ -> (interp, foldr (\(s, b) acc -> borraComplemento (if b then V s else NotV s) acc) clausulas interp)

-- Funcion auxiliar para red. Elimina la literal complemento dada una lista de clausulas
borraComplemento :: Literal -> [Clausula] -> [Clausula]
borraComplemento l = map (filter (/= complemento l))

-- Funcion auxiliar para red. Devuelve el complemento de una literal
complemento :: Literal -> Literal
complemento l = case l of
  (V s) -> NotV s
  (NotV s) -> V s

-- Funcion que aplica la regla de separacion (Split) del algoritmo DPLL.
sep :: Literal -> Estado -> (Estado, Estado)
sep l (interp, clausulas) = case l of
  (V s) -> ((interp ++ [(s, True)], clausulas), (interp ++ [(s, False)], clausulas))
  (NotV s) -> ((interp ++ [(s, True)], clausulas), (interp ++ [(s, False)], clausulas))

-- Funcion que dada un conjunto de clausulas, devuelve que literal aparece mas.
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral clausulas =
  let todasLasLiterales = concat clausulas
      gruposDeLiterales = group (sort todasLasLiterales)
      longitudesDeGrupos = map length gruposDeLiterales
      mayorGrupoIndex = snd (maximumBy (\x y -> compare (fst x) (fst y)) (zip longitudesDeGrupos [0 ..]))
      mayorGrupo = gruposDeLiterales !! mayorGrupoIndex
   in head mayorGrupo

------------------------------------------------------------------------------------------
-------------------------------ARBOL DPLL-------------------------------------------------
------------------------------------------------------------------------------------------

--Tipo de dato arbol binario para poder implementar el algoritmo DPLL
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving (Eq)

instance Show ArbolDPLL where
  show Void = "Void"
  show (Node estado Void) = "Node " ++ show estado ++ " Void "
  show (Node estado arbol) = "Node " ++ show estado ++ "\n" ++ "Arbol " ++ show arbol
  show (Branch estado izq der) = "Branch " ++ show estado ++ "\n" ++ "IZQ " ++ show izq ++ "\n" ++ "DER " ++ show der

--Funcion que recibe una formula proposicional en su forma clausular y devuelve una interpretacion que la satisface.
--En caso de que no exista una interpretacion que la satisfaga, devuelve la lista vacia.
dpll ::[Clausula] -> Interpretacion
dpll clausulas = if contieneComplemento clausulas then [] else
   let
      inicial = ([], clausulas)
      arbol = construyeArbolDPLL inicial
      resultado = modelo (buscaModelo arbol)
   in resultado

-- Funcion auxiliar para dpll. Recibe un estado y construye su arbol DPLL correspondiente.
construyeArbolDPLL :: Estado -> ArbolDPLL
construyeArbolDPLL (interp,clausulas) 
            |conflict (interp,clausulas) = Node ([],clausulas) Void
            |success (interp,clausulas) = Node (interp,clausulas) Void
            |unit (interp,clausulas) /= (interp,clausulas) = Node (interp,clausulas) (construyeArbolDPLL (unit (interp,clausulas)))
            |elim (interp,clausulas) /= (interp,clausulas) = Node (interp,clausulas) (construyeArbolDPLL (elim (interp,clausulas)))
            |red (interp,clausulas) /= (interp,clausulas) = Node (interp,clausulas) (construyeArbolDPLL (red (interp,clausulas)))
            | otherwise =
            let (izq, der) = sep (heuristicsLiteral clausulas) (interp, clausulas)
            in Branch (interp, clausulas) (construyeArbolDPLL izq) (construyeArbolDPLL der)


-- Función auxiliar para dpll. Explora el arbol DPLL generado y devuelve una interpretacion que satisfaga la formula (si existe), en otro caso devuelve [].
buscaModelo :: ArbolDPLL -> Interpretacion
buscaModelo Void = []
buscaModelo (Node (interp, clausulas) Void) = interp
buscaModelo (Node (interp, clausulas) arbol) = buscaModelo arbol
buscaModelo (Branch (interp, clausulas) izq der) = buscaModelo izq ++ buscaModelo der

-- Funcion auxiliar para dpll. Recibe un arbol DPLL y devuelve una interpretacion sin repeticion de literales.
modelo :: Interpretacion -> Interpretacion
modelo [] = []
modelo (x : xs) = x : modelo (filter (\(s, b) -> s /= fst x) xs)

--Funcion auxiliar para dpll. Dado un conjunto de clausulas, devuelve True si existe dos clausulas unitarias que son complementarias.
contieneComplemento :: [Clausula] -> Bool
contieneComplemento clausulas = any (\clausula -> length clausula == 1 && any (\clausula2 -> length clausula2 == 1 && complemento (head clausula) == head clausula2) clausulas) clausulas



