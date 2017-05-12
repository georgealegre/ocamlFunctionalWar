# Laboratorio 3 - La Guerra Funcional
# Lenguage - OCaml

## Alumno:
- Alegre, Jorge Facundo.

## Versión de OCaml (ocamlc y ocamlopt) usada:
4.03.0

### Como instalar:
Para poder jugar el juego, se debe tener `ocamlc` y `ocamlopt` instalados.
**ocamlc** es el compilador y _linker_ del código fuente a _bytecode_.
**ocamlopt** traduce este _bytecode_ a código máquina nativo.

Para saber si poseen estos programas pueden comprobarlo con la ejecución de los
mismos con el _flag_ `-v`. Si les aparece un número de versión y un directorio,
el programa está instalado.
`$ ocamlc -v`
`$ ocamlopt -v`

### Make:
Se provee un Makefile para compilar de manera rápida y sencilla.
Situados dentro del directorio `Lab4`, correr:
- `$ make` para compilar. Con el código entregado no deberían ocurrir problemas.
- `$ make play` para comenzar el juego.
- `$ make clean` para eliminar todos los archivos producidos por la compilación.

### Deciciones de diseño:
+ Las cartas especiales se las considera como las mejores cartas del juego.
