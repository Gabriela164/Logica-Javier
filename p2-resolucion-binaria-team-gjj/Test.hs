module Test where

import Parser
import Sat

mugiCheck :: Bool -> IO ()
mugiCheck a
  | a = putStrLn "Correcto! Kaizoku ou ni ore wa naru"
  | otherwise = putStrLn "Mal! Donificaste a Ace :C"

ej1 = parser $ lexer "(duerme ∧ corre) ∨ (¬duerme ∧ salta)"
ej2 = parser $ lexer "duerme ∧ ¬duerme"
ej3 = parser $ lexer "(ronca ↔ duerme) ∧ (¬ronca ↔ ¬duerme)"
ej4 = parser $ lexer "(p → q) ∧ p ∧ ¬q"
ej5 = parser $ lexer "((corre → suda) ∨ (corre → cansarse)) ↔ (corre ∧ ¬suda ∧ ¬cansarse)"
ej6 = parser $ lexer "T → ((p ↔ q) ∧ ((p ∧ ¬q) ∨ (¬p ∧ q) ∨ ⊥))"

t1 = saturacion ej1 == True  --revisar 
t2 = saturacion ej2 == False  --correcto
t3 = saturacion ej3 == True   --revisar 
t4 = saturacion ej4 == False  --correcto
t5 = saturacion ej5 == False  --correcto
t6 = saturacion ej6 == False  --revisar  (caso T y ⊥)

main = do
    putStrLn "Tests"
    mugiCheck t1
    mugiCheck t2
    mugiCheck t3
    mugiCheck t4
    mugiCheck t5
    mugiCheck t6
    