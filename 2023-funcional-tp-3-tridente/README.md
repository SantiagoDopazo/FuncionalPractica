# TP-3

## Tareas

- [ ] Aceptar el assignment y clonar el repositorio con el ejercicio
- [ ] Ir a la carpeta donde descargaste el ejercicio.
- [ ] Reemplazar la lista de integrantes con los nombres de los integrantes del equipo en el archivo README.md
- [ ] Resolver el ejercicio siguiendo [un esquema de trabajo](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md), eso incluye
- [ ] Ejecutar los tests con `stack test` y que den verde
- [ ] A medida que vas resolviendo el ejercicio, subir [el progreso a git](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/git/resolverConflictos.md)

## Grupo, nombre y legajo de integrantes

- Nombre del grupo: **Completar**

Integrantes:
- Legajo: 1758744 
- Nombre: Santiago Agustin Dopazo

- Legajo: 1775066
- Nombre: Agustin Santiago Coda

- Legajo: **Completar** 
- Nombre: **Completar**

## Pre-requisitos

Necesitás haber instalado el ambiente según se explica en el TP-0.

## Ayuda

Si tienen dudas con Haskell pueden ayudarse todo el tiempo con esta documentación

- [Guía de lenguajes](https://docs.google.com/document/d/1oJ-tyQJoBtJh0kFcsV9wSUpgpopjGtoyhJdPUdjFIJQ/edit?usp=sharing), un resumen de las principales funciones que vienen con Haskell.
- [Hoogle](https://www.haskell.org/hoogle/), un motor de búsqueda específico para Haskell.

Aparte, siempre pueden preguntar a sus ayudantes en discord!

Y para comenzar a trabajar con Git les recomendamos [este apunte inicial de Git](https://docs.google.com/document/d/1ozqfYCwt-37stynmgAd5wJlNOFKWYQeIZoeqXpAEs0I/edit) o estos videos donde se explica como usar Git:
- [Parte 1: Qué es GIT y cómo clonar el repo basado en GitHub classroom](https://www.youtube.com/watch?v=rRKe7l-ZNvM)
- [Parte 2: Uso básico de GIT con status, add, reset, commit, push](https://www.youtube.com/watch?v=OgasfM5qJJE)
- [Parte 3: Resolución de conflictos](https://www.youtube.com/watch?v=sKcN7cWFniw)

### Probando cosas por consola

La forma que recomendamos de resolver el ejercicio es no programar todo de una y después ver si anda, si no ir probando en cada paso a medida que van programando cada función.

Para esto, les recomiendo que usen mucho `stack ghci` para probar cosas por consola, y vuelvo a linkear [esta página](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md#comandos-%C3%BAtiles) donde se explican un par de cositas de como usar `ghci`.

### Testeo automatizado

Nuestra solución tiene que estar escrita en el archivo `Library.hs` del directorio `src`, entonces podemos correr pruebas **automatizadas** en la terminal:

```bash
stack test
```

También pueden usar ghcid para correr los tests automáticamente como se explica [acá](https://github.com/pdepviernestm/2021-clases/blob/main/clase-02/correrTestsMasRapidoConGhcid.md).

## El enunciado

En este TP van a tener que definir **usando recursividad** (no vale usando otras funciones que hagan la recursividad por ustedes) las siguientes funciones y tiparlas y **escribir ustedes los tests**:

### Parte 1: recursividad

- Definir la funcion **fibonacci**, que dado un número, devuelva el término correspondiente en la serie de Fibonacci.

  La serie de Fibonacci es una sucesión infinita de números en la que cada término es la suma de los dos términos anteriores. La serie comienza con los números 0 y 1, y los términos siguientes se calculan sumando los dos términos anteriores. Por ejemplo, los primeros 10 términos de la serie son: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34.

- Definir una función **rellenar** que dado un string, un número y un caracter, agrega ese caracter al final del string hasta que el string resultante sea de longitud número. Si el string ya era de tamaño mayor o igual al número entonces esta función devuelve el string como se le pasó.
Ej:
```haskell
> rellenar "hola" 7 '!'
"hola!!!"
```

- Implementar la función **dividir**, que recibe un dividendo y un divisor, usando un algoritmo de restas sucesivas. El algoritmo, explicado de manera imperativa, funciona de la siguiente manera:
   - Paso 1: Necesitamos un contador de cuantas veces se restó hasta llegar al resultado, así que necesitamos un contador de restas que comienza en 0.
   - Paso 2: Si el dividendo es menor al divisor, se retorna el contador.
   - Paso 3: Si no, restar el divisor al dividendo, y aumentar en 1 el contador de restas.
   - Paso 4: Volver al paso 2. 

  Adaptarlo al paradigma funcional.

### Parte 2: listas

- **ultimaCarta**: recibe una lista y devuelve el último elemento de la lista.

- **primeras**: recibe una lista y un número n y devuelve una nueva lista con los primeros n elementos de la lista original.

- **cartasAColores**: recibe una lista de cartas y devuelve una lista de los colores de esas cartas, en el mismo orden en el que se encontraban las cartas.
  Por ej:
  ```haskell
  > cartasAColores [CartaNumerica 1 Rojo, CartaNumerica 2 Verde, CartaNumerica 3 Rojo]
  [Rojo, Verde, Rojo]
  ```

- **obtenerElemento**: recibe un numero y una lista, y devuelve el elemento con ese indice en la lista. Ej:
```haskell
> obtenerElemento 0 [1, 2, 3]
1.0
> obtenerElemento 2 [1, 2, 3, 4, 5]
3.0
> obtenerElemento 1 ["hola", "chau"]
"chau"
```

- **sacarHastaEncontrar**: recibe una lista de cartas y una carta, devuelve todas las cartas en la lista hasta encontrar la carta buscada. Ej:
```haskell
> sacarHastaEncontrar (CartaNumerica 5 Rojo) [CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 5 Rojo, CartaNumerica 4 Amarillo, CartaNumerica 0 Verde]
[CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 5 Rojo]
```
Si la carta no está, devuelve todas las cartas pasadas.

- **lasRojas**: dada una lista de cartas, retorna una lista con solo aquellas que son rojas. Ej:
```haskell
> lasRojas [CartaNumerica 5 Rojo, CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 4 Amarillo]
[CartaNumerica 5 Rojo, CartaEspecial Reversa Rojo]
```

- **lasQueSonDeColor**: dada una lista de cartas y un color, retorna una lista con solo aquellas que son de ese color. Ej:
```haskell
> lasQueSonDeColor Azul [CartaNumerica 5 Rojo, CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 4 Amarillo]
[CartaNumerica 2 Azul]
```

- **lasFiguras**: dada una lista de cartas, retorna una lista con solo aquellas que tengan figuras. Ej:
```haskell
> lasFiguras [CartaNumerica 5 Rojo, CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 4 Amarillo]
[CartaEspecial Reversa Rojo]
```

- **sumatoria**: dado una lista de números, los suma. Si la lista está vacía devuelve 0. No usar `sum` que es literalmente la misma función que queremos definir :). Ej:
```haskell
> sumatoria [1,2,3,4,5,6]
21
> sumatoria []
0
```


## Que hacer cuando terminan el TP

Cuando terminen, creen un issue etiquetando a sus tutores así les llega una notificación y se corrigen y les dejan feedback ahí.
![](https://i.imgur.com/ypeXpBw.gif)
