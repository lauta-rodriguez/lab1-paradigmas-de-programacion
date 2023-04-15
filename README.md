# Laboratorio de programación funcional

## Integrantes:

### Grupo 22

- Lara Kurtz, lara.kurtz@mi.unc.edu.ar
- Gonzalo Bordon, gonzalobordon02@mi.unc.edu.ar
- Lautaro Rodri­guez, lautaro.rodriguez@mi.unc.edu.ar

# Introducción

En este trabajo se implementa un lenguaje de dominio específico (DSL) en Haskell para reproducir una version simplificada del dibujo de M.C. Escher.

# Ejecución

### Compilar y correr Escher:

```shell
ghc -o Main Main.hs && ./Main Escher
```

### Limpiar el directorio al terminar la ejecución:

```shell
rm Main *.o *.hi Dibujos/*.hi Dibujos/*.o
```

# Testing

Incorporamos un conjunto de tests para verificar el correcto funcionamiento de las funciones implementadas en los módulos `Dibujo.hs` y `Pred.hs`.

Para llevar a cabo estos tests, se ha utilizado el framework de pruebas unitarias **HUnit**.

**HUnit** es una librería para Haskell que nos proporciona una sintaxis clara para describir y ejecutar pruebas.

Utilizar una librería para encargarnos de esta tarea nos facilita el mantenimiento del código a largo plazo.

### Compilar y correr tests:

```shell
ghc -o Test Test.hs && ./Test
```

### Limpiar el directorio al terminar la ejecución:

```shell
rm Test *.o *.hi Tests/*.hi Tests/*.o
```

# Preguntas

- ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.

> La separación de las funcionalidades en los distintos módulos nos permite entender de forma más clara cómo se relacionan los diferentes componentes del lenguaje, la interpretación geometrica y los usos especificos del mismo.

> De esta forma, el módulo `Dibujo` se encarga de definir la sintaxis del lenguaje de figuras (los constructores y las funciones que podemos usar) mientras que el módulo `Interp` define la semántica del lenguaje, es decir, la interpretación geometrica de las figuras.

> En el módulo `Escher` se utiliza el lenguaje para reproducir el famoso dibujo de Escher, mientras que en el módulo `Pred` se definen las funciones de predicado que nos permiten evaluar las propiedades de las figuras.

- ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez es un parámetro del tipo?

> Las figuras básicas no están incluidas en la definición del lenguaje para tener la flexibilidad de poder definir el tipo de figura básica que queremos utilizar, ya que hay muchos tipos de figuras básicas. Por esto es que definimos un tipo de dato polimórfico para definir las figuras básicas.

- ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?

> La ventaja de utilizar la función fold en lugar de hacer pattern-matching directo es que fold permite definir una operación de acumulación genérica y aplicarla a cada elemento de la lista, lo cual resulta en un código más genérico y modular.

# Triángulo de Sierpinski

Buscando en internet ejemplos de fractales, encontramos que el triángulo de Sierpinski es un fractal que se puede generar con un triángulo y 3 puntos interiores. Para generarlo se debe repetir el proceso con los 3 puntos interiores del triángulo anterior.

Tal y como estaba construido el lenguaje, no se podía generar un triángulo de Sierpinski, ya que no se podía generar un triángulo con cada sub-triangulo del mismo tamaño (había deformaciones en el superior), por lo que se tuvo que implementar el operador `escalar` para modificar el tamaño de la figura sin alterar su espacio asignado ni posición (refiriéndonos a la esquina inferior izquierda).

### Para correr el programa:

```shell
ghc -o Main Main.hs && ./Main Fractal
```

# Animaciones

Se agregó un nuevo parámetro de tipo `Float` que indica el momento de la animación, para que se puedan generar nuevas figuras. Son animaciones de tipo **Frame a Frame**, por lo que son relativamente sencillas. Se podría añadir también un operador `Mover Float Float` para hacer más fácil el animado de movimiento y que sea más "Nativo" a nuestra librería, de todas formas no es indispensable.

### Se deja un ejemplo de animación (poco elaborado) que se puede ejecutar con el comando:

```bash shell
ghc -package GLUT -package gloss Main.hs && ./Main AnimateRandom
```
