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
