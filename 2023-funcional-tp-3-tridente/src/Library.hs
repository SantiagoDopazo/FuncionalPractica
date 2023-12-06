module Library where
import PdePreludat
import GHC.IO.Handle.Types (Handle__(Handle__))

data Color = Rojo | Verde | Amarillo | Azul deriving(Show,Eq)
data Simbolo = Reversa | Mas4 | SaltarTurno deriving (Eq, Show)

data Carta =
    CartaNumerica { numero :: Number, color :: Color } |
    CartaEspecial { simbolo :: Simbolo, color :: Color }
    deriving (Eq, Show)

fibonacci :: Number -> Number
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n
    | n>=2 = fibonacci(n-1) + fibonacci (n-2)

rellenar :: String -> Number -> Char -> String
rellenar string numero caracter
 | length string >= numero = string
 | otherwise = rellenar (agregarAlFinal caracter string) numero caracter

agregarAlFinal :: Char -> String -> String
agregarAlFinal caracter string = string ++ [caracter]

dividir :: Number -> Number -> Number
dividir dividendo divisor
    | dividendo < divisor = 0
    | otherwise = 1 + dividir (dividendo-divisor) divisor

ultimaCarta :: [Carta]-> Carta
ultimaCarta (cabeza:[]) = cabeza
ultimaCarta (cabeza:cola) = ultimaCarta cola  

primeras :: Number->[a]->[a]
primeras i _
    | i <= 0 = []
primeras  _ [] = []
primeras  i (x:xs)  = (x : primeras (i-1) xs)

cartasAColores ::[Carta] -> [Color]
cartasAColores []=[]
cartasAColores (CartaNumerica _ color:cartas) = (color:cartasAColores cartas)
cartasAColores (CartaEspecial _ color:cartas) = (color:cartasAColores cartas)
    
obtenerElemento :: Number -> [a] -> a
obtenerElemento _ [] = error "lista vacia"
obtenerElemento n (x:xs)
  | n == 0    = x
  | otherwise = obtenerElemento (n-1) xs

sacarHastaEncontrar :: Carta -> [Carta] -> [Carta]
sacarHastaEncontrar _ [] = []
sacarHastaEncontrar cartaBuscada (x:xs)
  | cartaBuscada == x = [x]
  | otherwise = x : sacarHastaEncontrar cartaBuscada xs

lasRojas::[Carta]->[Carta]
lasRojas cartas = filter (\c -> color c == Rojo) cartas

lasQueSonDeColor :: Color->[Carta]-> [Carta]
lasQueSonDeColor colorBuscado cartas = filter (\c -> color c == colorBuscado) cartas

{-lasQueSonDeColor ::Color->[Carta]->[Carta]
lasQueSonDeColor _ []=[]
lasQueSonDeColor buscar (CartaNumerica numero color:cartas)
    |buscar == color =(CartaNumerica numero color:lasQueSonDeColor buscar cartas)
    |otherwise = lasQueSonDeColor buscar cartas
lasQueSonDeColor buscar (CartaEspecial simbolo color:cartas)
    |buscar == color =(CartaEspecial simbolo color:lasQueSonDeColor buscar cartas)
    |otherwise = lasQueSonDeColor buscar cartas-}

-- filtrar :: a->[a]->[a]
-- filtrar _ [] = []
-- filtrar filtro (x:xs)
--     |filtro x = x:filtrar filtro xs
--     |otherwise = filtrar filtro xs

esCartaEspecial :: Carta -> Bool
esCartaEspecial (CartaEspecial simbolo _) = True
esCartaEspecial _ = False

lasFiguras :: [Carta]->[Carta]
lasFiguras [] = []
lasFiguras (x:xs)
    |esCartaEspecial x = x:lasFiguras xs
    |otherwise = lasFiguras xs

sumatoria :: [Number]->Number
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
