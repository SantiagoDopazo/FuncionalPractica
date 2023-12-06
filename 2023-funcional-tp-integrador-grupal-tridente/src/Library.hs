module Library where
import PdePreludat
import GHC.Num (Num)

data Color = ROJO | NEGRO | AZUL | VERDE
    deriving (Eq,Show)

data Posicion = NORTE | SUR | ESTE | OESTE
    deriving (Eq,Show)

data Celda = Celda{ 
    bolitas :: [Color]
} deriving (Eq,Show)

data Tablero = Tablero{
    celdas :: [[Celda]]
}deriving (Eq,Show)

data Cabezal = Cabezal {
    coordenada :: (Number, Number),
    tablero :: Tablero
}deriving (Eq,Show)

celdaVacia :: Celda
celdaVacia = Celda[]

iniciarTablero :: Number->Tablero
iniciarTablero numero = Tablero (replicate numero (replicate numero celdaVacia))

crearTablero :: Number->Cabezal
crearTablero numero = Cabezal (numero-1, 0) (iniciarTablero numero)

--MOVIMIENTO 

mover :: Posicion -> Cabezal -> Cabezal
mover posicion (Cabezal (fila, columna) tablero)
    |esPosicionValida (direccion posicion (fila, columna)) (dimensionesTablero tablero) = moverCabezal posicion (Cabezal (fila, columna) tablero)
    |otherwise = error  "El cabezal se cayo del tablero"

direccion :: Posicion->(Number,Number)->(Number, Number)
direccion OESTE (fila, columna) = (fila, columna-1)
direccion ESTE (fila, columna) = (fila, columna+1) 
direccion SUR (fila, columna) = (fila+1, columna) 
direccion NORTE (fila, columna) =(fila-1, columna)

dimensionesTablero :: Tablero -> Number
dimensionesTablero (Tablero celdas) = length celdas

esPosicionValida :: (Number, Number) ->Number -> Bool
esPosicionValida (fila, columna) numeroCeldas = fila >= 0 && fila < numeroCeldas && columna >= 0 && columna < numeroCeldas

moverCabezal :: Posicion->Cabezal->Cabezal
moverCabezal posicion (Cabezal (fila, columna) tablero) = Cabezal (direccion posicion (fila, columna)) tablero

irAlBorde :: Posicion -> Cabezal -> Cabezal
irAlBorde posicion cabezal@(Cabezal (fila, columna) tablero)
    |esPosicionValida (direccion posicion (fila, columna)) (dimensionesTablero tablero) = irAlBorde posicion (moverCabezal posicion cabezal) 
    |otherwise = cabezal

--Poner Bolita 

poner :: Color -> Cabezal -> Cabezal
poner color cabezal = tableroModificadoPorColor color cabezal

celdaActual :: Tablero->(Number,Number)->Celda
celdaActual tablero (fila,columna) = celdas tablero !! fila !! columna

agregarBolita :: Celda -> Color -> Celda
agregarBolita celda color = Celda (color: bolitas celda)

tableroModificadoPorColor:: Color -> Cabezal -> Cabezal
tableroModificadoPorColor color (Cabezal (fila, columna) tablero) = Cabezal (fila,columna) (Tablero (nuevoTablero(agregarBolita(celdaActual tablero (fila,columna)) color) (fila,columna) tablero))

nuevoTablero :: Celda -> (Number,Number) -> Tablero -> [[Celda]]
nuevoTablero celda (fila, columna) tablero =
    let filaActual = celdas tablero !! fila
        nuevaFila = take columna filaActual ++ [celda] ++ drop (columna + 1) filaActual
        nuevoTablero = take fila (celdas tablero) ++ [nuevaFila] ++ drop (fila + 1) (celdas tablero)
    in nuevoTablero

cantidadDeBolitas :: Color -> Cabezal -> Number
cantidadDeBolitas color (Cabezal (fila, columna) tablero)  = contarBolitas color (Cabezal (fila, columna) tablero)

contarBolitas :: Color -> Cabezal -> Number
contarBolitas color (Cabezal (fila, columna) tablero) = length (filter (== color) (bolitas (celdaActual tablero (fila,columna))))
 
--Sacar Bolita

sacar :: Color -> Cabezal -> Cabezal
sacar color cabezal
    |hayBolitasDeColor color cabezal = tableroModificadoPorSacarColor color cabezal
    |otherwise = error "No hay bolitas del color pedido para sacar de la celda actual"

eliminarElemento :: Eq a => a -> [a] -> [a]
eliminarElemento _ [] = []
eliminarElemento elemento (x:xs)
    | elemento == x = xs  
    | otherwise = x : eliminarElemento elemento xs 

hayBolitasDeColor :: Color -> Cabezal -> Bool
hayBolitasDeColor color (Cabezal (fila, columna) tablero) = elem color (bolitas(celdaActual tablero (fila,columna)))

sacarBolita :: Celda -> Color -> Celda
sacarBolita celda color = Celda (eliminarElemento color (bolitas celda))

tableroModificadoPorSacarColor:: Color -> Cabezal -> Cabezal
tableroModificadoPorSacarColor color (Cabezal (fila, columna) tablero) = Cabezal (fila,columna) (Tablero (nuevoTablero(sacarBolita(celdaActual tablero (fila,columna)) color) (fila,columna) tablero))


-- Funciones de repeticion de sentencias

aplicarFunciones :: [Cabezal -> Cabezal] -> Cabezal -> Cabezal
aplicarFunciones funciones cabezal = foldl (flip ($)) cabezal funciones

programa :: [Cabezal -> Cabezal] -> Cabezal -> Cabezal
programa = aplicarFunciones

repetir :: Number -> [Cabezal -> Cabezal] -> Cabezal -> Cabezal
repetir cantidadDeRepeticiones funciones cabezal = aplicarFunciones (concat (replicate cantidadDeRepeticiones funciones)) cabezal

repetirSegunCondicion :: (Cabezal -> Bool) -> [Cabezal -> Cabezal] -> Cabezal -> Cabezal 
repetirSegunCondicion condicion sentencias cabezal
    | condicion cabezal = programa sentencias cabezal
    | otherwise = cabezal

--Repite Logica
alternativa :: (Cabezal -> Bool) -> [Cabezal -> Cabezal] -> [Cabezal -> Cabezal] -> Cabezal -> Cabezal
alternativa condicion sentenciasVerdaderas sentenciasFalsas cabezal = si condicion sentenciasVerdaderas (siNo condicion sentenciasFalsas cabezal)
    -- | condicion cabezal = programa sentenciasVerdaderas cabezal
    -- | otherwise = programa sentenciasFalsas cabezal

si :: (Cabezal -> Bool) -> [Cabezal -> Cabezal] -> Cabezal -> Cabezal
si = repetirSegunCondicion

siNo :: (Cabezal -> Bool) -> [Cabezal -> Cabezal] -> Cabezal -> Cabezal
siNo condicion = repetirSegunCondicion (not.condicion)

--Repite logica
mientras :: (Cabezal -> Bool) -> [Cabezal -> Cabezal] -> Cabezal -> Cabezal
mientras condicion sentencias cabezal
    | condicion cabezal = mientras condicion sentencias (programa sentencias cabezal)
    | otherwise = cabezal


tableroFinal = programa [mover NORTE, poner NEGRO, poner NEGRO, poner AZUL, mover NORTE,repetir 15 [poner ROJO,poner AZUL],si (hayBolitasDeColor VERDE) [mover ESTE,poner NEGRO], siNo (hayBolitasDeColor VERDE) [mover SUR,mover ESTE,poner AZUL],mover ESTE,mientras (\cabezal -> cantidadDeBolitas VERDE cabezal <= 9) [poner VERDE],poner AZUL] (crearTablero 3)
        