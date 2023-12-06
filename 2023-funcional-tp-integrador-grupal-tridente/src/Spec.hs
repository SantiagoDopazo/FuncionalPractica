module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    suiteDeTestsDeMovimiento
    suiteDeTestsDeModificarTablero
    suiteDeTestsDeFuncionalidades
suiteDeTestsDeMovimiento = describe "Movimiento" $ do

        describe "Mover" $ do
            it "Dada una posición, le indicamos la direccion a donde nos gustaria movernos" $ do
                mover NORTE (crearTablero 3) `shouldBe` Cabezal {coordenada = (1,0), tablero = Tablero {celdas = [[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}]]}}
            it "Dada una posición, le indicamos la direccion a donde nos gustaria movernos pero está en el borde del tablero" $ do
                mover SUR (crearTablero 3) `shouldBe` error  "El cabezal se cayo del tablero"
        describe "Moverme al borde" $ do
            it "Dada una posición, le indicamos una posicion para que se mueva hasta el borde del tablero" $ do
                irAlBorde NORTE (crearTablero 3) `shouldBe` Cabezal {coordenada = (0,0), tablero = Tablero {celdas = [[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}]]}}
suiteDeTestsDeModificarTablero = describe "Modificar Tablero" $ do
        describe "Agregar bolitas de color" $ do
            it "Le indicamos de que color son las bolitas que queremos agregar en la celda actual correspondiente" $ do
                poner AZUL (crearTablero 3) `shouldBe` Cabezal {coordenada = (2,0), tablero = Tablero {celdas = [[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = [AZUL]},Celda {bolitas = []},Celda {bolitas = []}]]}}
        describe "Contar bolitas" $ do
            it "Contamos cuantas bolitas hay de un color en la celda actual" $ do
                cantidadDeBolitas AZUL (programa [poner AZUL, poner AZUL] (crearTablero 3)) `shouldBe` 2
        describe "Sacar bolitas de color" $ do
            it "Le indicamos de que color son las bolitas que queremos eliminar en la celda correspondiente" $ do
                sacar VERDE (programa [poner VERDE] (crearTablero 3))`shouldBe` Cabezal {coordenada = (2,0), tablero = Tablero {celdas = [[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}]]}}
            it "Le indicamos de que color son las bolitas que queremos eliminar en la celda correspondiente pero no hay bolitas de ese color" $ do
                sacar NEGRO (crearTablero 3)`shouldBe` error "No hay bolitas del color pedido para sacar de la celda actual"
suiteDeTestsDeFuncionalidades = describe "Funciones Utiles" $ do
        describe "Programa" $ do
            it "Recibe un tablero y una lista de acciones y nos devuelve el tablero modificado por las acciones" $ do
                programa [mover NORTE, poner NEGRO, poner AZUL, mover SUR, poner VERDE] (crearTablero 3) `shouldBe` Cabezal {coordenada = (2,0), tablero = Tablero {celdas = [[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = [AZUL,NEGRO]},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = [VERDE]},Celda {bolitas = []},Celda {bolitas = []}]]}}
            it "Recibe un tablero,una condicion y unas sentencias si las cumple las realiza" $ do
                si (hayBolitasDeColor VERDE) [mover ESTE,poner NEGRO] (programa [poner VERDE] (crearTablero 3)) `shouldBe` Cabezal {coordenada = (2,1), tablero = Tablero {celdas = [[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = [VERDE]},Celda {bolitas = [NEGRO]},Celda {bolitas = []}]]}}
            it "Recibe un tablero,una condicion y unas sentencias si no las cumple las realiza" $ do
                siNo (hayBolitasDeColor VERDE) [mover ESTE,mover NORTE,poner AZUL] (programa [poner AZUL] (crearTablero 3)) `shouldBe` Cabezal {coordenada = (1,1), tablero = Tablero {celdas = [[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = [AZUL]},Celda {bolitas = []}],[Celda {bolitas = [AZUL]},Celda {bolitas = []},Celda {bolitas = []}]]}}
            it "Recibe un tablero una condicion y unas sentencias mientras cumpla la conidicion realizara las sentencias" $ do
                mientras (\cabezal -> cantidadDeBolitas VERDE cabezal <= 9) [poner VERDE] (crearTablero 3) `shouldBe` Cabezal {coordenada = (2,0), tablero = Tablero {celdas = [[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = [VERDE,VERDE,VERDE,VERDE,VERDE,VERDE,VERDE,VERDE,VERDE,VERDE]},Celda {bolitas = []},Celda {bolitas = []}]]}}
            it "Recibe un tablero una condicion y dos sentencias si se cumple se hace las primeras sentencias si no se cumplen se hacen las segundas" $ do
                alternativa (hayBolitasDeColor VERDE) [mover ESTE,poner NEGRO] [mover NORTE,poner ROJO] (programa [poner VERDE] (crearTablero 3)) `shouldBe` Cabezal {coordenada = (2,1), tablero = Tablero {celdas = [[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = []},Celda {bolitas = []},Celda {bolitas = []}],[Celda {bolitas = [VERDE]},Celda {bolitas = [NEGRO]},Celda {bolitas = []}]]}}