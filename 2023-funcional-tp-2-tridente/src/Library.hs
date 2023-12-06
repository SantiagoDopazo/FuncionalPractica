module Library where
import PdePreludat

-- 1. Tuplas
type Tupla = (Number,String,Number)
type Tuplanum = (Number,Number)

first :: (a,b,c) ->a
first (a,_,_) = a

second :: (a,b,c)-> b
second (_,b,_) = b

third :: (a,b,c) -> c
third (_,_,c) = c

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

divisionConResto :: (Number,Number) -> (Number,Number)
divisionConResto (num1,num2)= (div num1 num2, rem num1 num2)

-- 2. Titulos
data Titulo = Ingenieria | Licenciatura | Doctorado | Ninguno
    deriving(Eq,Show)
data Persona =  UnaPersona String String Number Titulo
    deriving(Eq,Show)
--type Persona = (String,String,Number,Titulos)            

agustin :: Persona
agustin = UnaPersona "Agustin" "Coda" 20 Ninguno

edad :: Persona -> Number
edad (UnaPersona _ _ edad _) = edad

titulo :: Persona->Titulo
titulo (UnaPersona _ _ _  untitulo) = untitulo

prefijoTitulo :: Titulo->String
prefijoTitulo titulo
    |titulo == Ingenieria = "Ing." 
    |titulo == Licenciatura = "Lic." 
    |titulo == Doctorado =  "Doc."
    |otherwise = ""

concatenacionDosStrings :: String->String->String
concatenacionDosStrings string1 string2 = string1 ++ string2

concatenacionDosStringsEspacio :: String->String->String
concatenacionDosStringsEspacio string1 string2 = string1 ++" "++ string2

nombreCompleto :: Persona->String
nombreCompleto (UnaPersona nombre apellido edad titulo) = concatenacionDosStrings (prefijoTitulo titulo) (concatenacionDosStringsEspacio nombre apellido)

{-nombreCompleto :: Persona->String
nombreCompleto (UnaPersona nombre apellido edad titulo) 
    |titulo == Ingenieria = "Ing." ++ nombreApellido (UnaPersona nombre apellido edad titulo)
    |titulo == Licenciatura = "Lic." ++ nombreApellido (UnaPersona nombre apellido edad titulo)
    |titulo == Doctorado =  "Doc." ++ nombreApellido (UnaPersona nombre apellido edad titulo)
    |titulo == Ninguno = nombreApellido (UnaPersona nombre apellido edad titulo) -}


sumaEdad :: Persona->Titulo->Number
sumaEdad (UnaPersona _ _ edad _) unTitulo = edad + tiempoCarrera unTitulo 

tiempoCarrera :: Titulo->Number
tiempoCarrera Ingenieria = 6
tiempoCarrera Licenciatura = 4
tiempoCarrera Doctorado = 2
tiempoCarrera Ninguno = 0

Developer



recibirse :: Titulo->Persona->Persona
recibirse unTitulo (UnaPersona nombre apellido edad titulo) = UnaPersona nombre apellido (sumaEdad (UnaPersona nombre apellido edad titulo)unTitulo) unTitulo
-- 3. Pregunta

--Si yo tengo a un estudiante que tiene 26 años, se llama Juan Fernandes y tiene título de Ingeniería:
juan :: Persona 
juan = UnaPersona "Juan" "Fernandez" 26 Ingenieria -- Escriban el código de como sería

-- y en ghci evaluo el código que haría que juan se reciba de un doctorado

-- Y luego le pido a ghci que evalue

-- edad juan

-- ¿cuantos años me va a devolver esa ultima consulta? ¿Por qué?

-- Respuesta: Juan sigue teniendo 26 ya que se esta cambiando el dato en la funcion recibirse pero el constructor siempre va a estar harcodeado con 26 ya que en haskell son variables inmutables no se pueden cambiar como en C al menos no sin una libreria especial  

-- 4. Devs
practicar = implementame
