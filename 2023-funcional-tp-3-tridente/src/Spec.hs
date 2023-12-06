module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)
unoRojo = CartaNumerica 1 Rojo
dosVerde = CartaNumerica 2 Verde
tresAzul = CartaNumerica 3 Azul
cuatroAmarillo = CartaNumerica 4 Amarillo 
manoVacia :: [Carta]
manoVacia = []
listaDeColoresVacia :: [Color]
listaDeColoresVacia = []
correrTests :: IO ()
correrTests = hspec $ do
  describe "fibonacci" $ do
    it "el primer numero de la serie es 0" $ do
      fibonacci 0 `shouldBe` 0
    it "el segundo numero de la serie es 1" $ do
      fibonacci 1 `shouldBe` 1
    it "cada numero desde el tercero es la suma de los dos numeros anteriores" $ do
      fibonacci 2 `shouldBe` 1
      fibonacci 3 `shouldBe` 2
      fibonacci 4 `shouldBe` 3
      fibonacci 5 `shouldBe` 5

  describe "rellenar" $ do
    it "dado un caracter, un numero y un string de longitud igual al numero, retorna ese string" $ do
      rellenar "hola" 4 '!' `shouldBe` "hola"
    it "dado un caracter, un numero y un string de longitud menor al numero, retorna ese string" $ do
      rellenar "hola" 3 '!' `shouldBe` "hola"
    it "dado un caracter, un numero y un string de longitud mayor al numero, retorna un nuevo string de longitud igual al numero que se obtiene concatenando el caracter al final del string original varias veces" $ do
      rellenar "hola" 7 '!' `shouldBe` "hola!!!"

  describe "dividir" $ do
    it "si el divisor que es mayor al dividendo da 0" $ do
      dividir 3 8 `shouldBe` 0
    it "si el divisor es menor al dividendo devuelve la division entera entre ambos" $ do
      dividir 8 3 `shouldBe` 2

  describe "ultimaCarta" $ do
    it "si le pasamos una lista con solo una carta, nos devuelve esa carta" $ do
      ultimaCarta [unoRojo] `shouldBe` unoRojo
    it "si le pasamos una lista con cartas, nos devuelve la ultima" $ do
      ultimaCarta [unoRojo, dosVerde] `shouldBe` dosVerde

  describe "primeras" $ do
    it "si le pasamos 0, nos devuelve una lista vacia sin importar las cartas que le hayamos pasado" $ do
      primeras 0 [unoRojo, dosVerde] `shouldBe` manoVacia
    it "si le pasamos una lista vacia, nos devuelve una lista vacia" $ do
      primeras 2 manoVacia `shouldBe` manoVacia
    it "si le pedimos mas cartas de las que hay en la lista, nos devuelve la lista entera" $ do
      primeras 5 [unoRojo, dosVerde] `shouldBe` [unoRojo, dosVerde]
    it "si le pasamos 1, nos devuelve una lista con el primer elemento" $ do
      primeras 1 [unoRojo, dosVerde] `shouldBe` [unoRojo]
    it "nos devuelve una lista con las primeras n cartas" $ do
      primeras 3 [unoRojo, dosVerde, tresAzul, cuatroAmarillo] `shouldBe` [unoRojo, dosVerde, tresAzul]
  
  describe "obtenerElemento" $ do
    it "retorna el elemento que esta en la posicion pasada" $ do
      obtenerElemento 1 [1, 2, 3] `shouldBe` 2
    it "falla si se pasa una posicion negativa" $ do
      deberiaFallar (obtenerElemento (-1) [1])
    it "falla si se pasa una lista vacia" $ do
      deberiaFallar (obtenerElemento 0 [])
    it "falla si se pasa una posicion para la cual no hay elemento" $ do
      deberiaFallar (obtenerElemento 2 [1])


  describe "sacarHastaEncontrar" $ do 
    it "sacarHastaEncontrar devuelve una lista hasta la carta buscada inclusive" $ do
      sacarHastaEncontrar (CartaNumerica 5 Verde) [CartaNumerica 5 Rojo, CartaNumerica 2 Azul,CartaNumerica 5 Verde, CartaEspecial Reversa Rojo, CartaNumerica 9 Verde] `shouldBe` [CartaNumerica 5 Rojo, CartaNumerica 2 Azul,CartaNumerica 5 Verde]
    it "sacarHastaEncontrar devuelve una lista vacia si no se encuentra el elemento pedido" $ do
      sacarHastaEncontrar (CartaNumerica 1 Amarillo) [CartaNumerica 2 Azul, CartaNumerica 3 Rojo] `shouldBe` []
  
  describe "lasRojas" $ do
    it "lasRojas devuelve solamente las cartas rojas" $ do
      lasRojas [CartaNumerica 5 Rojo, CartaNumerica 2 Azul, CartaEspecial Mas4 Rojo] `shouldBe` [CartaNumerica 5 Rojo, CartaEspecial Mas4 Rojo]
    it "lasRojas devuelve una lista vacia si no hay al menos una carta roja" $ do
      lasRojas [CartaNumerica 2 Azul, CartaNumerica 7 Amarillo] `shouldBe` []
  
  describe "lasQueSonDeColor" $ do
    it "LasQueSonDeColor devuelve solamente las cartas Azules" $ do
      lasQueSonDeColor Azul [CartaEspecial Reversa Azul, CartaNumerica 2 Azul, CartaNumerica 9 Azul, CartaNumerica 4 Amarillo] `shouldBe` [CartaEspecial Reversa Azul,CartaNumerica 2 Azul, CartaNumerica 9 Azul]
    it "LasQueSonDeColor devuelve solamente las cartas Verdes" $ do
      lasQueSonDeColor Verde [CartaNumerica 2 Azul, CartaNumerica 3 Rojo] `shouldBe` []
 
  describe "lasFiguras" $ do
    it "devuelve una carta especial de la lista" $ do
      lasFiguras [CartaNumerica 1 Rojo,CartaEspecial Reversa Azul] `shouldBe` [CartaEspecial Reversa Azul]
    it "devuelve una lista vacia si no hay una figura" $ do
      lasFiguras [CartaNumerica 1 Rojo, CartaNumerica 2 Verde] `shouldBe` [] 
  
  describe "sumatoria" $ do
    it "el resultado de una lista con numeros es la suma de cada uno de ellos" $ do
      sumatoria [1,2,3,4,5] `shouldBe` 15
    it "el resultado de una lista vacia es 0" $ do 
      sumatoria [] `shouldBe` 0

-- Funcion auxiliar definida para testear en obtenerElemento
deberiaFallar :: a -> Expectation
deberiaFallar unaExpresion = evaluate unaExpresion `shouldThrow` anyException