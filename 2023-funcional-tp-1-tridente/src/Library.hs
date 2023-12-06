{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat
import GHC.Num (Num)

-- 1. Numeros

siguiente :: Number -> Number
siguiente n = n+1

esPositivo :: Number -> Bool
esPositivo n = n>0

inversa :: Number -> Number
inversa n = 1/ n

-- 2. Temperaturas

celsiusAFarenheit :: Number -> Number
celsiusAFarenheit celsius = celsius * 1.8 +32

farenheitACelsius :: Number -> Number
farenheitACelsius farenheit = (farenheit-32)/1.8

haceFrioCelsius :: Number -> Bool
haceFrioCelsius grados = grados<=8

haceFrioFarenheit :: Number -> Bool
haceFrioFarenheit grados = haceFrioCelsius (farenheitACelsius grados)

-- 2.5 Bonus OPCIONAL
perimetroCirculo :: Number -> Number
perimetroCirculo radio = 3.14*2*radio

perimetroCuadrado :: Number -> Number
perimetroCuadrado lado = 4*lado

superficieCuadrado :: Number -> Number
superficieCuadrado lado = lado*lado

superficieCubo :: Number -> Number
superficieCubo lado = 6*superficieCuadrado lado

superficieCilindro :: Number -> Number -> Number
superficieCilindro radio altura = perimetroCirculo radio *altura + perimetroCirculo radio * radio

-- 3. Mas numeros!

max' :: Number->Number->Number
max' num1 num2
    |num1>num2 = num1
    |otherwise = num2
min' :: Number -> Number -> Number
min' num1 num2 
    |num1<num2 = num1
    |otherwise = num2

-- 3.5 Bonus OPCIONAL
cuantosDiasTiene :: Number -> Number
cuantosDiasTiene anio
    |mod anio 400 == 0 = 366
    |mod anio 4 == 0 && mod anio 100 /=0 = 366
    |otherwise = 365

precioTotal :: Number -> Number -> Number
precioTotal valorUnitario cantidad
    |cantidad<3 = valorUnitario * cantidad
    |cantidad>=3 && cantidad<10 = (0.9*valorUnitario)*cantidad
    |cantidad>=10 = (0.7*valorUnitario)*cantidad

-- 4. Pinos

alturaLimiteDePino :: Number
alturaLimiteDePino = 3

alturaHasta :: Number->Number->Number
alturaHasta alturaLimiteDePino alturaPino = min alturaLimiteDePino alturaPino

alturaSobre :: Number->Number->Number
alturaSobre alturaLimiteDePino alturaPino = max 0 (alturaPino - alturaLimiteDePino)

pesoParteInferior :: Number->Number
pesoParteInferior alturaPino = alturaHasta alturaLimiteDePino alturaPino * 100 * 3

pesoParteSuperior :: Number->Number
pesoParteSuperior alturaPino = alturaSobre alturaLimiteDePino alturaPino * 100 * 2

pesoPino :: Number->Number
pesoPino alturaPino = pesoParteInferior alturaPino + pesoParteSuperior alturaPino

esPesoUtil :: Number->Bool
esPesoUtil kg = kg>=400 && kg<=1000
    --pesoPino(altura)<=1000 && pesoPino(altura)>=400

sirvePino :: Number->Bool
sirvePino altura = esPesoUtil(pesoPino altura)
