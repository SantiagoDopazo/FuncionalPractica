module Library where
import PdePreludat

data Animal = Animal {
    energia :: Number,
    tipo :: Tipo,
    peso :: Number
} deriving(Show, Eq)

data Tipo = Volador | Terrestre | Acuatico deriving(Show, Eq)

-- Parte 1: Animales

losDeTipo :: Tipo->[Animal]->[Animal]
losDeTipo tipo animales  = filter(\animal -> coincidenTipos animal tipo) animales

coincidenTipos :: Animal->Tipo->Bool
coincidenTipos (Animal _ tipo _) unTipo = tipo==unTipo

esHambriento :: Animal->Bool
esHambriento (Animal energia _ _) = energia<10

animalesHambrientos :: [Animal]->[Animal]
animalesHambrientos animales= filter esHambriento animales

-- Parte 2: Alimentos y entrenamientos

-- implementar estos alimentos:
-- bayas aumenta la energia en 5 y el peso en 0.1
-- carne aumenta la energia en 20 y el peso en 2

bayas :: Animal->Animal
bayas (Animal energia tipo peso) = Animal (energia+5) tipo (peso+0.1)

carne:: Animal -> Animal
carne (Animal energia tipo peso) = Animal (energia+20) tipo (peso+2)

entrenamientoTerreste :: Animal->Animal
entrenamientoTerreste (Animal energia tipo peso) = Animal (energia-5) tipo (peso-5)

entrenamientoVolador :: Animal->Animal
entrenamientoVolador (Animal energia tipo peso) = Animal energia tipo (peso-3)

entrenar :: Animal->Animal
entrenar animal
    |coincidenTipos animal Terrestre = entrenamientoTerreste animal
    |coincidenTipos animal Volador = entrenamientoVolador animal
    |otherwise = animal

alimentarATodos :: (Animal->Animal)->[Animal]->[Animal]
alimentarATodos comida animales = map comida animales

aplicarItinerario ::[Animal -> Animal]->Animal -> Animal
aplicarItinerario itinerario animal = foldl (\animal unaTarea->unaTarea animal) animal itinerario 

-- Parte 3: Nuestras propias funciones de orden superior

mapTupla :: (a->b)->(a,a)->(b,b)
mapTupla funcion (x,y) =(funcion x,funcion y) 

menorSegun :: Ord b=>(a->b)->a->a->a
menorSegun funcion x y = comparar funcion y x

minimoSegun :: Ord b=>(a->b)->[a]->a
minimoSegun funcion lista = foldl (comparar funcion) (last lista) lista

comparar :: Ord b=>(a->b)->a->a->a
comparar funcion siguiente acumulado
  | funcion siguiente < funcion acumulado = siguiente
  | otherwise = acumulado

aplicarVeces :: Number->(a->a)->a->a
aplicarVeces 0 _ valor = valor
aplicarVeces numero funcion valor = aplicarVeces (numero-1) funcion (funcion valor)

replicar :: Number->a->[a]
replicar numero valor = aplicarVeces numero (\lista ->lista  ++ [valor]) []

-- Parte 4. Bonus: combinando funciones

(|>) :: a ->(a->b)->b
valor |> funcion = funcion valor

vocales:: [Char]
vocales = ['a','e','i','o','u','A','E','I','O','U']

esVocal :: Char->Bool
esVocal letra =  letra |> (\unaletra -> elem unaletra vocales)

saltoDeLinea :: Char
saltoDeLinea = '\n'

primeraLinea :: String -> String
primeraLinea frase = frase |> takeWhile(/= saltoDeLinea) 

lasVocales :: String -> String
lasVocales string = filter esVocal string

-- definir usando |> para combinar las funciones
contarVocalesDeLaPrimeraLinea :: String -> Number
contarVocalesDeLaPrimeraLinea frase = frase |> primeraLinea |> lasVocales |> length