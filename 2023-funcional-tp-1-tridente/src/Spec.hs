module Spec where
import PdePreludat
import Library
import Test.Hspec
import           Control.Monad (unless)
import Test.Hspec.Runner (Config(..), hspecWith, defaultConfig)
import Test.Hspec.Formatters (progress, specdoc)

correrTests :: IO ()
correrTests = runHspec $ do
  focus suiteDeTestsDeParteI
  suiteDeTestsDeParteIBonus
  focus suiteDeTestsDeParteII
  focus suiteDeTestsDeParteIII
  suiteDeTestsDeParteIII
  suiteDeTestsDeParteIIIBonus
  focus suiteDeTestsDeParteIV
  suiteDeTestsDeParteIV
  
suiteDeTestsDeParteI =
  describe "Parte I: Numeros" $ do

    describe "siguiente" $ do
      it "el siguiente de un numero es el numero + 1" $ do
        siguiente (-1) `shouldBe` 0
        siguiente 0 `shouldBe` 1
        siguiente 1 `shouldBe` 2

    describe "inversa" $ do
      it "la inversa de 1 es 1" $ do
        inversa 1 `shouldBe` 1
      it "la inversa de cualquier numero es el resultado de dividir 1 por ese numero" $ do
        inversa 4 `shouldBe` 0.25
        inversa 0.25 `shouldBe` 4

    describe "esPositivo" $ do
      it "es verdad para los numeros mayores a 0" $ do
        esPositivo 1 `shouldBe` True
      it "es falso para los numeros menores a 0" $ do
        esPositivo (-1) `shouldBe` False
      it "es falso para el 0" $ do
        esPositivo 0 `shouldBe` False

suiteDeTestsDeParteIBonus =
  describe "Parte 3 Bonus" $ do
    it "el perimetro de un circulo es 2 * pi * radio" $ do
      perimetroCirculo 2.5 `shouldBeEqualUpTo2Decimals` 15.70796
    it "el perimetro de un cuadrado es el lado x 4" $ do
      perimetroCuadrado 3 `shouldBe` 12
    it "la superficie de un cuadrado es el lado al cuadrado" $ do
      superficieCuadrado 3 `shouldBe` 9
    it "la superficie de un cubo es el area de una cara por la cantidad de caras (6)" $ do
      superficieCubo 3 `shouldBe` 54
    it "la superficie de un cilindro es el area de las tapas por el area de la pared del cilindro" $ do
      superficieCilindro 2 4 `shouldBeEqualUpTo2Decimals` 75.39822

suiteDeTestsDeParteII =
  describe "Parte 2: Temperaturas" $ do
    describe "celsiusAFarenheit" $ do
      it "pasa una temperatura de celsius a farenheit" $ do
        celsiusAFarenheit 7 `shouldBeEqualUpTo2Decimals` 44.6

    describe "farenheitACelsius" $ do
      it "pasa una temperatura de farenheit a celsius" $ do
        farenheitACelsius 70 `shouldBeEqualUpTo2Decimals` 21.1111

      describe "farenhetiACelsius y celsiusAFarenheit son inversas" $ do
        it "convertir un valor en celsius a farenheit y luego volver a convertir a celsius retorna el valor original" $ do
          farenheitACelsius(celsiusAFarenheit 35) `shouldBe` 35
        it "convertir un valor en farenheit a celsius y luego volver a convertir a farenheit retorna el valor original" $ do
          celsiusAFarenheit(farenheitACelsius 44.6) `shouldBe` 44.6
      

    describe "haceFrioCelsius" $ do
      -- Pista: hay 3 casos a testear
      it "Es verdad cuando hacen menos de 8 grados celsius" $ do
        haceFrioCelsius 7 `shouldBe` True
      it "Es verdad cuando hace 8 grados celsius" $ do
        haceFrioCelsius 8 `shouldBe` True
      it "Es falso cuando hacen mas de 8 grados celsius" $ do
        haceFrioCelsius 9 `shouldBe` False

    describe "haceFrioFarenheit" $ do
      -- Pista: hay 3 casos a testear
      it "Es verdad cuando hacen menos de 46.4 grados farenheit" $ do
        haceFrioFarenheit 46.3 `shouldBe` True
      it "Es verdad cuando hace 46.4 grados farenheit" $ do
        haceFrioFarenheit 46.4 `shouldBe` True
      it "Es falso cuando hacen mas de 46.4 grados farenheit" $ do
        haceFrioFarenheit 46.5 `shouldBe` False
        

suiteDeTestsDeParteIII =
  describe "Parte 3: Mas Numeros!" $ do

    describe "max'" $ do
      -- Pista: hay 3 casos a testear
      it "7 es mayor que 5" $ do
        max' 7 5 `shouldBe` 7
      it "-2 es mayor que -6" $ do
         max' (-6) (-2) `shouldBe` (-2)
      it "7 es igual a 7" $ do
        max' 7 7 `shouldBe` 7
    describe "min'" $ do
      it "5 es menor que 7" $ do
        min' 5 7 `shouldBe` 5
      it "-6 es menor que -2" $ do
        min' (-2) (-6) `shouldBe` (-6)
      it "7 es igual a 7" $ do
        min' 7 7 `shouldBe` 7

suiteDeTestsDeParteIIIBonus =
  describe "Parte 3 Bonus" $ do
    describe "cuantosDiasTiene" $ do
      it "devuelve 365 días para un año no bisiesto" $ do
        cuantosDiasTiene 2023 `shouldBe` 365

      it "devuelve 366 días para un año bisiesto" $ do
        cuantosDiasTiene 2024 `shouldBe` 366
    
    describe "precioTotal" $ do
      it "retorna el precio total para menos de 3 productos" $ do
        precioTotal 100 2 `shouldBe` 200

      it "retorna el precio total con un descuento del 10% para entre 3 y 10 productos" $ do
         precioTotal 100 3 `shouldBe` 270

      it "retorna el precio total con un descuento del 30% cuando se compran de a mas de 10 productos" $ do
        precioTotal 100 10 `shouldBe` 700

suiteDeTestsDeParteIV =
  describe "Parte 4: Pinos" $ do

    describe "pesoPino" $ do
      it "El peso de un arbol menor a 3 metros" $ do
        pesoPino 2 `shouldBe` 600
      it "El peso de un arbol mayor a 3 metros" $ do
        pesoPino 5 `shouldBe` 1300
      it "El peso de un pino de 3 metros" $ do
        pesoPino 3 `shouldBe` 900  

    describe "esPesoUtil" $ do
      it "Un pino que pesa menos de 400 kilos no es util"  $ do
       esPesoUtil 300 `shouldBe` False
      it "Un pino que pesa entre 400 y 1000 kilos es util" $ do
        esPesoUtil 600 `shouldBe` True
      it "Un pino que pesa más de 1000 kilos no es util" $ do
        esPesoUtil 1500 `shouldBe` False

    describe "sirvePino" $ do
      it "Un pino que tenga un peso menor a 400 kilos no sirve" $ do
        sirvePino 1 `shouldBe` False
      it "Un pino que tenga un peso entre 400 y 1000 kilos sirve" $ do
        sirvePino 3 `shouldBe` True
      it "Un pino que tenga un peso mayor a 1000 kilos no sirve" $ do
        sirvePino 6 `shouldBe` False    

escribiTestsParaEstaFuncion :: SpecWith ()
escribiTestsParaEstaFuncion = pure ()

shouldBeEqualUpTo2Decimals :: Number -> Number -> Expectation
shouldBeEqualUpTo2Decimals aNumber anotherNumber = shouldBeEqualWithErrorLessThan 0.01 aNumber anotherNumber

shouldBeEqualWithErrorLessThan :: Number -> Number -> Number -> Expectation       
shouldBeEqualWithErrorLessThan error aNumber anotherNumber
  | aNumber - anotherNumber < error = pure () -- Esto hace que el test de verde!
  | otherwise = expectationFailure (show aNumber ++ " no es igual (comparando con error < " ++ show error ++ ") a " ++ show anotherNumber)

runHspec :: Spec -> IO ()
runHspec tests = hspecWith defaultConfig {configFormatter = Just specdoc} tests
