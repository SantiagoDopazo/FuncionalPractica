module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDePartes1y2
  suiteDeTestsDeParte3
  suiteDeTestsDeParte4 -- descomenta esto solo si vas a hacer el punto bonus

suiteDeTestsDePartes1y2 = describe "Animales, alimentos y entrenamientos" $ do
  -- les dejamos estos valores definidos que probablemente les vengan bien
  let tigre = Animal 5 Terrestre 120
  let lechuza = Animal 40 Volador 10
  let tiburon = Animal 100 Acuatico 100
  let ningunAnimal :: [Animal]
      ningunAnimal = []
  describe "los de un tipo" $ do
    -- y este test :)
    it "dada una lista vacia retorna la misma lista vacia" $ do
      losDeTipo Volador [] `shouldBe` ningunAnimal
    it "retorna los animales de un mismo tipo" $ do
      losDeTipo Volador [tigre, lechuza, tiburon] `shouldBe` [lechuza]

  describe "animalesHambrientos" $ do 
    it "dada una lista vacia retorna la misma lista vacia" $ do
      animalesHambrientos [] `shouldBe` ningunAnimal
    it "Retorna los animales hambrientos con menos de 10 de enrgia" $ do
      animalesHambrientos [tigre, lechuza, tiburon] `shouldBe` [tigre]

  describe "alimentos" $ do
    it "una baya aumenta en 5 la energia de un animal y su peso +0.1" $ do
      bayas tigre `shouldBe` Animal 10 Terrestre 120.1
    it "la carne aumenta en 20 la energia de un animal y su peso +2" $ do
      carne tiburon `shouldBe` Animal 120 Acuatico 102  
   
  describe "alimentarATodos" $ do
    it "dada una lista de animales y una comida se alimenta a todos" $ do
      alimentarATodos carne [tigre,lechuza,tiburon] `shouldBe` [Animal 25 Terrestre 122, Animal 60 Volador 12, Animal 120 Acuatico 102]
    it "si se da una lista vacia sin importar el alimento se devuelve una lista vacia" $ do
      alimentarATodos bayas [] `shouldBe` []

  describe "entrenar" $ do
    it "si se entrena a un animal terrestre se disminuye su energia y su peso en 5" $ do
      entrenar tigre `shouldBe` Animal 0 Terrestre 115
    it "si se entrena a un animal volador solo baja su peso en 3" $ do
      entrenar lechuza `shouldBe` Animal 40 Volador 7
    it "un animal acuatico no se puede entrenar por lo que no hace nada" $ do
      entrenar tiburon `shouldBe` tiburon    

  describe "aplicar itinerario" $ do
    it "se puede aplicar un intinerario de entrenamientos y comidas" $ do
      aplicarItinerario [entrenar,carne,bayas,entrenar] lechuza `shouldBe` Animal 65 Volador 6.1
    it "si se da un itinerario vacio a un animal no ocurre nada" $ do
      aplicarItinerario [] tigre `shouldBe` tigre

suiteDeTestsDeParte3 = describe "Orden Superior" $ do
  describe "mapTupla" $ do
    it "Recibe dos elementos y le aplica la funcion correspondiente" $ do
      mapTupla length ("Hola", "mundo") `shouldBe` (4, 5)

  describe "menorSegun" $ do
    -- Traten de probar que pasa si el primer elemento es mayor segun el criterio,
    -- si el segundo es mayor y si son iguales
    it "si el primero es mayor devuelve el segundo " $ do
      menorSegun length "Anillos" "Señor" `shouldBe` "Señor"
    it "si el segundo es mayor devuelve el primero " $ do
      menorSegun length "Frodo" "Bolson" `shouldBe` "Frodo"
    it "si son iguales devuelve el primero " $ do
      menorSegun length "Gandalf" "Saruman" `shouldBe` "Gandalf"
    
  describe "minimoSegun" $ do
    it "dada una lista y una funcion devuelve el menor segun el criterio de la funcion" $ do
      minimoSegun length ["Zelda", "Link", "Trifuerza"] `shouldBe` "Link"
    it "dada una lista y una funcion devuelve el menor segun el criterio de la funcion" $ do
      minimoSegun energia [Animal 5 Terrestre 120, Animal 40 Volador 10, Animal 100 Acuatico 100] `shouldBe` Animal 5 Terrestre 120

  describe "aplicarVeces" $ do
    it "dado un numero una funcion y un valor aplica la funcion el numero determinado de veces a la funcion" $ do
      aplicarVeces 3 (\texto -> texto ++ "ni") "The Knights who say" `shouldBe` "The Knights who sayninini"

  describe "replicar" $ do
    it "dado un numero y un valor crea una lista con el valor la cantidad de veces segun el numero" $ do
      replicar 4 "LOTR" `shouldBe` ["LOTR", "LOTR", "LOTR", "LOTR"]
    it "Dado 0 y un valor devuelve una lista vacia" $ do
      replicar 0 True `shouldBe` []
      
suiteDeTestsDeParte4 = describe "combinando funciones" $ do
  -- Los tests de acá se los dejamos servidos :)
  describe "|>" $ do
    it "dado un valor y una funcion, pasa el valor como parametro de la funcion" $ do
      "hola" |> length `shouldBe` 4
      3 |> (\n -> n + 2) `shouldBe` 5

  describe "esVocal" $ do
    it "dada una letra vocal da True" $ do
      esVocal 'a' `shouldBe` True
      esVocal 'E' `shouldBe` True
    it "dada una letra consontante da False" $ do
      esVocal 'c' `shouldBe` False
      esVocal 'B' `shouldBe` False
    it "dado un caracter que no es una letra da False" $ do
      esVocal ' ' `shouldBe` False
      esVocal '@' `shouldBe` False

  describe "primeraLinea" $ do
    it "dado un texto sin saltos de linea, devuelve el mismo texto" $ do
      primeraLinea "hola mundo!" `shouldBe` "hola mundo!"
    it "dado un texto con saltos de linea, devuelve el mismo hasta antes del primer salto de linea" $ do
      primeraLinea "hola\nmundo!" `shouldBe` "hola"

  describe "lasVocales" $ do
    it "dado un texto vacio, devuelve un string vacio" $ do
      lasVocales "" `shouldBe` ""
    it "dado un texto sin vocales, devuelve un string vacio" $ do
      lasVocales "why" `shouldBe` ""
    it "dado un texto con vocales, devuelve un string con solo las vocales" $ do
      lasVocales "chau" `shouldBe` "au"
      lasVocales "azarath metrion zinthos" `shouldBe` "aaaeioio"

  describe "contarVocalesDeLaPrimerLinea" $ do
    it "dado un texto vacio, da 0" $ do
      contarVocalesDeLaPrimeraLinea "" `shouldBe` 0
    it "dado un texto sin saltos de linea, da la cantidad de vocales en ese texto" $ do
      contarVocalesDeLaPrimeraLinea "hello world" `shouldBe` 3
    it "dado un texto sin saltos de linea, da la cantiad de vocales hasta el primer salto de linea" $ do
      contarVocalesDeLaPrimeraLinea "Aeea,\n yo soy sabalero" `shouldBe` 4

escribime :: Expectation
escribime = implementame

deberiaFallar :: a -> Expectation
deberiaFallar expresion = evaluate expresion `shouldThrow` anyException
