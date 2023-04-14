# Laboratorio de programación funcional

## Integrantes:

- Lara Kurtz, lara.kurtz@mi.unc.edu.ar
- Gonzalo Bordon, gonzalobordon02@mi.unc.edu.ar
- Lautaro Rodri­guez, lautaro.rodriguez@mi.unc.edu.ar

# Introducción

# Desarrollo

# Ejecución

Compilar y correr en ghc:

```bash
ghc -o Main Main.hs && ./Main Escher
```

Limpiar el directorio al terminar la ejecución:

```bash
rm Main *.o *.hi Dibujos/*.hi Dibujos/*.o
```

# Testing

# Preguntas

- ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.

- ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez es un parámetro del tipo?

- ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?

# Triángulo de Sierpinski
- Buscando en internet ejemplos de fractales, encontramos que el triángulo de Sierpinski es un fractal que se puede generar con un triángulo y 3 puntos interiores. Para generarlo se debe repetir el proceso con los 3 puntos interiores del triángulo anterior.

> Tal y como estaba construido el lenguaje, no se podía generar un triángulo de Sierpinski, ya que no se podía generar un triángulo con cada sub-triangulo del mismo tamaño (había deformaciones en el superior), por lo que se tuvo que implementar el operador `escalar` para modificar el tamaño de la figura sin alterar su espacio asignado ni posición (refiriéndonos a la esquina inferior izquierda).

# Animaciones
> Se agregó un nuevo parámetro de tipo `Float` que indica el momento de la animación, para que se puedan generar nuevas figuras. Son animaciones de tipo **Frame a Frame**, por lo que son relativamente sencillas. Se podría añadir también un operador `Mover Float Float` para hacer más fácil el animado de movimiento y que sea más "Nativo" a nuestra librería, de todas formas no es indispensable.

Se deja un ejemplo de animación (poco elaborado) que se puede ejecutar con el comando 

```bash
ghc -package GLUT -package gloss Main.hs && ./Main AnimateRandom
```
