module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | Pativegano | Panintegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente Pativegano = 10
precioIngrediente Panintegral =3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


doblecuartoPollo =Hamburguesa 20 [Pan,Pollo,Cheddar,Pollo,Cheddar,Pan]
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]
doblecuarto= Hamburguesa 20 [Pan,Carne,Cheddar,Carne,Cheddar,Pan]
bigPdep= Hamburguesa 20 [Pan,Carne,Cheddar,Carne,Cheddar,Curry,Pan]
doblecuartovegano= Hamburguesa 20 [Panintegral, Pativegano, QuesoDeAlmendras,Pativegano,QuesoDeAlmendras,Panintegral]

tieneCarne:: [Ingrediente]->Bool
tieneCarne ingredientes = elem  Carne ingredientes 

elementosBase::[Ingrediente]->Bool
elementosBase ingredientes = tieneCarne ingredientes || elem Pollo ingredientes

basehamburguesa:: Hamburguesa->Ingrediente->Bool
basehamburguesa (Hamburguesa precioBase ingredientes) ingredienteabuscar =  elem ingredienteabuscar ingredientes

precioIngredientes:: [Ingrediente]->Number
precioIngredientes ingredientes = (sum.map precioIngrediente) ingredientes

precioHamburguesa:: Hamburguesa->Number
precioHamburguesa hamburguesa = precioBase hamburguesa + precioIngredientes (ingredientes hamburguesa)

agregarIngrediente :: Hamburguesa->Ingrediente->Hamburguesa
agregarIngrediente (Hamburguesa precioBase ingredientes) unIngrediente =  Hamburguesa precioBase (ingredientes ++ [unIngrediente])

agrandar :: Hamburguesa->Hamburguesa
agrandar hamburguesa 
   |basehamburguesa hamburguesa Pollo = agregarIngrediente hamburguesa Pollo
   |otherwise= agregarIngrediente hamburguesa Carne

descuento :: Hamburguesa->Number->Hamburguesa
descuento (Hamburguesa precioBase ingredientes) porcentaje = Hamburguesa (aplicarDescuento precioBase porcentaje) ingredientes   

aplicarDescuento:: Number->Number->Number
aplicarDescuento valor porcentaje = valor-valor*porcentaje/100


delDia:: Hamburguesa->Hamburguesa
delDia hamburguesa = ((`descuento` 30) . (`agregarIngrediente` Papas)) hamburguesa 

cambiarIngredientes :: Ingrediente->Ingrediente->[Ingrediente]->[Ingrediente]
cambiarIngredientes ingredienteACambiar ingredienteCambiante ingredientes = map (cambiarSiCorresponde ingredienteACambiar ingredienteCambiante) ingredientes

cambiarSiCorresponde :: Ingrediente -> Ingrediente -> Ingrediente -> Ingrediente
cambiarSiCorresponde ingredienteACambiar ingredienteCambiante ingrediente
  | ingrediente == ingredienteACambiar = ingredienteCambiante
  | otherwise = ingrediente

hacerVeggie ::Hamburguesa->Hamburguesa
hacerVeggie (Hamburguesa precioBase ingredientes)=  Hamburguesa precioBase (cambiarIngredientes Pollo Pativegano (cambiarIngredientes Carne Pativegano ingredientes)) 


cambiarPanDePati::Hamburguesa->Hamburguesa
cambiarPanDePati (Hamburguesa precioBase ingredientes) = Hamburguesa precioBase (cambiarIngredientes Pan Panintegral ingredientes)