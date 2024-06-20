module Prop where

data Prop = Var String | Cons Bool| Not Prop
          | And Prop Prop | Or Prop Prop
          | Impl Prop Prop | Syss Prop Prop
          deriving (Show, Eq)

-- Funcion fnn dada una formula proposicional la convierte en 
-- su forma normal negativa (FNN)
fnn :: Prop -> Prop
fnn (Var x) = Var x
fnn (Cons x) = Cons x
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Impl p q) = Or (fnn (Not p)) (fnn q)
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))
--negaciones de formulas proposicionales
fnn (Not p) = case p of 
                Var x   -> Not (Var x)
                Not p   -> fnn p
                And p q -> Or (fnn (Not p)) (fnn (Not q))
                Or p q  -> And (fnn (Not p)) (fnn (Not q))
                Impl p q -> And (fnn p) (fnn (Not q))
                Syss p q -> fnn (And (Impl p q) (Impl q p))
                


-- Funcion fnc dada una formula proposicional la convierte en
-- su forma normal conjuntiva (FNC)
fnc :: Prop -> Prop
fnc p = case fnn p of
          And q r  -> And (fnc q) (fnc r)
          Or  q r  -> distribuida (fnc q) (fnc r)
          Var x    -> p
          Not(Var x) -> Not(Var x)
         
--Funcion aux para fnc, aplica las leyes de distributividad
distribuida :: Prop -> Prop -> Prop 
distribuida (And p q) r = (And (distribuida p r) (distribuida q r))
distribuida p (And q r) = (And (distribuida p q) (distribuida p r))
distribuida p q = (Or p q) 






