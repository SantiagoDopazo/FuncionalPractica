module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
   suiteDeTestsDeParteI
suiteDeTestsDeParteI =
    describe "ParteI: hamburguesas" $ do

        


        describe "precioIngredientes" $ do
            it "el precio de carne, cheddar y dos panes es de 34" $ do
                precioIngredientes [Pan,Cheddar,Carne,Pan] `shouldBe` 34

        describe "precioHamburguesa " $ do        
            it "el precio final de un cuarto del dia es 54" $ do
                precioHamburguesa cuartoDeLibra `shouldBe` 54

        describe "agregarIngrediente" $ do
            it "dada un cuarto de libra y curry, se le agrega el ingrediente a la hamburguesa" $ do
                agregarIngrediente cuartoDeLibra Curry `shouldBe` (Hamburguesa 20 [Pan,Carne,Cheddar,Pan,Curry])
        
        describe "descuento" $ do
            it "dada un cuarto de libra con un descuento del 20%, su precio base seria de 16" $ do
                descuento cuartoDeLibra 20 `shouldBe` (Hamburguesa 16 [Pan,Carne,Cheddar,Pan])
            it "dada una hamburguesa con un precio base de 40 y un descuento de 50, su precio seria de 20" $ do
                descuento (Hamburguesa 40 [Pan,Carne,Cheddar,Carne,Curry,Pan]) 50 `shouldBe` (Hamburguesa 20 [Pan,Carne,Cheddar,Carne,Curry,Pan])
        describe "agrandar" $ do
            it "dada una hamburguesa con carne, se le va a sumar carne" $ do
               agrandar cuartoDeLibra `shouldBe` Hamburguesa 20 [Pan,Carne,Cheddar,Pan,Carne]
            it "dada una hamburguesa con pollo, se le suma pollo" $ do  
                agrandar doblecuartoPollo `shouldBe` Hamburguesa 20 [Pan,Pollo,Cheddar,Pollo,Cheddar,Pan,Pollo]


        describe "delDia" $ do
            it "el precio final de una doblecuarto del dia es 88" $ do
                (precioHamburguesa.delDia) doblecuarto `shouldBe` 88
            it "a una doble cuarto de libra del dia se le agregan papas" $ do    
                delDia doblecuarto `shouldBe` Hamburguesa 14 [Pan,Carne,Cheddar,Carne,Cheddar,Pan,Papas]

        describe "hacerVeggie" $ do         
         it "dada una hamburguesa con carne la transforma a una hamburguesa con pati vegano" $ do
            hacerVeggie cuartoDeLibra `shouldBe` Hamburguesa 20 [Pan,Pativegano,Cheddar,Pan]
         it "dada una hamburguesa con pollo la transforma a una hamburguesa con pati vegano" $ do 
            hacerVeggie doblecuartoPollo `shouldBe` Hamburguesa 20 [Pan,Pativegano,Cheddar,Pativegano,Cheddar,Pan]  
         it " dada una hamburguesa de carne y pollo la transforma en una hamburguesa con pati vegano" $ do 
             hacerVeggie (Hamburguesa 20 [Pan,Carne,Cheddar,Pollo,Pan]) `shouldBe` Hamburguesa 20 [Pan,Pativegano,Cheddar,Pativegano,Pan]

        describe "cambiarPanDePati" $ do 
            it "dada una hamburguesa con pan normal, cambia los panes por pan integral" $ do 
                cambiarPanDePati cuartoDeLibra `shouldBe` Hamburguesa 20 [Panintegral,Carne,Cheddar,Panintegral]  