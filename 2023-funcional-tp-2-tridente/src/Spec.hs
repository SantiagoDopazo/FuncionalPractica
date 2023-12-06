module Spec where
import PdePreludat
import Library
import Test.Hspec
import           Control.Monad (unless)

correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParteI
  suiteDeTestsDeParteII
  suiteDeTestsDeParteIV

suiteDeTestsDeParteI =
  describe "ParteI: Tuplas" $ do

    describe "first" $ do
      it "first de una tupla de 3 elementos, me da el primero" $ do
        first (1, "dos", 3) `shouldBe` 1
        first ("dos", True,8 ) `shouldBe` "dos"
        first (True, 20, "diez")`shouldBe` True
    

    describe "second" $ do
      it "second de una tupla de 3 elementos, me da el segundo" $ do
        second ("uno", False, 3) `shouldBe` False
        second (True, "trece", 8) `shouldBe` "trece"
        second ("nueve",5,False) `shouldBe` 5 
        

    describe "third" $ do
      it "third de una tupla de 3 elementos, me da el tercero" $ do
        third (False, "dos", 3) `shouldBe` 3
        third ("cinco", 13, True) `shouldBe` True
        third (8,True,"veinte")  `shouldBe` "veinte"

    describe "swap" $ do
      it "swap de una tupla con elementos iguales, me da la misma tupla" $ do
        swap (0, 0) `shouldBe` (0, 0)
      it "swap me transforma una tupla dando vuelta los elementos de lugar" $ do
        swap (1, 2) `shouldBe` (2, 1)
        swap ("quince", "hola") `shouldBe` ("hola", "quince")

    describe "divisionConResto" $ do
      it "divisionConResto de un dividendo dividible por su divisor, me da el resultado de la division y 0" $ do
        divisionConResto (4,2) `shouldBe` (2, 0)
      it "divisionConResto de un dividendo no divisible por su divisor, me da el resultado de la division y el resto" $ do
        divisionConResto (4,3) `shouldBe` (1, 1)

suiteDeTestsDeParteII =
  describe "ParteII: Titulos" $ do

    describe "edad" $ do
      it "dado un estudiante, nos devuelve su edad" $ do
        edad (UnaPersona "Agustin" "Coda" 21 Ninguno ) `shouldBe` 21


    describe "nombreCompleto" $ do
      it "si se pasa alguien sin título, devuelve el nombre y el apellido" $ do
        nombreCompleto (UnaPersona "Agustin" "coda" 21 Ninguno) `shouldBe` "Agustin coda"
      it "si se pasa alguien con ingenieria, devuelve el nombre y el apellido precedidos por Ing." $ do
        nombreCompleto (UnaPersona "Pepe" "Rodriguez" 26 Ingenieria) `shouldBe` "Ing.Pepe Rodriguez"
      it "si se pasa alguien con licenciatura, devuelve el nombre y el apellido precedidos por Lic." $ do
        nombreCompleto(UnaPersona "Juan" "Gomez" 27 Licenciatura) `shouldBe` "Lic.Juan Gomez"
      it "si se pasa alguien con doctorado, devuelve el nombre y el apellido precedidos por Doc." $ do
        nombreCompleto (UnaPersona "Martin" "Samuel" 28 Doctorado) `shouldBe` "Doc.Martin Samuel"

    describe "recibirse" $ do
      it "hacer que se recibia alguien con un doctorado nos devuelve a esa persona con 2 años más y con título de Doctorado" $ do
        recibirse Doctorado (UnaPersona "Santiago" "Dopazo" 21 Ninguno) `shouldBe` UnaPersona "Santiago" "Dopazo" 23 Doctorado 
      it "hacer que se recibia alguien con una licenciatura nos devuelve a esa persona con 4 años más y con título de Licenciatura" $ do
        recibirse Licenciatura (UnaPersona "Roberto" "Garcia" 30 Ingenieria) `shouldBe` UnaPersona "Roberto" "Garcia" 34 Licenciatura
      it "hacer que se recibia alguien con una ingenieria nos devuelve a esa persona con 6 años más y con título de Ingenieria" $ do
        recibirse Ingenieria (UnaPersona "Gabriel" "Dopazo" 60 Licenciatura) `shouldBe` UnaPersona "Gabriel" "Dopazo" 66 Ingenieria

suiteDeTestsDeParteIV =
  describe "ParteIV: Devs" $ do

    describe "recibirse" $ do
      it "hacer que se recibia alguien como Dev con 0 años de experiencia nos devuelve a esa persona con 2 años mas y con titulo de Dev de 0 años de experiencia" $ do
        pending
      it "hacer que se reciba alguien como Dev con ciertos años de experiencia nos devuelve a esa persona con esos años de experiencia + 2 y con título de Dev de esos años de experiencia" $ do
        pending

    describe "nombreCompleto" $ do
      it "si se pasa alguien con título de Dev con 0 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Jr." $ do
        pending
      it "si se pasa alguien con título de Dev con 1 año de experiencia, devuelve el nombre y el apellido precedidos por Dev Jr." $ do
        pending
      it "si se pasa alguien con título de Dev con 2 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Ssr." $ do
        pending
      it "si se pasa alguien con título de Dev con entre 2 años y 5 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Ssr." $ do
        pending
      it "si se pasa alguien con título de Dev con 5 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Ssr." $ do
        pending
      it "si se pasa alguien con título de Dev con más de 5 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Sr." $ do
        pending

    describe "practicar" $ do
      it "si hago practicar ciertos años a alguien con titulo de Dev de 0 años de experiencia, me devuelve una persona con esa cantidad de años de experiencia en su título y cuya edad aumento en la cantidad de años de practica" $ do
        pending
      it "si hago practicar ciertos años a alguien con titulo de Dev con algunos años de experiencia, me devuelve una persona cuya experiencia en su título es lo que tenía más lo que le hice practicar y cuya edad aumento en la cantidad de años de practica" $ do
        pending
      it "hacer practicar a una persona no Dev cierta cantidad de años me devuelve esa persona con esa cantidad de años más pero sin ningún otro cambio" $ do
        pending