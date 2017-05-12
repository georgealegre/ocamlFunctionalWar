#Laboratorio 3: Programación Funcional en OCaml

En este laboratorio hay que codificar un juego de cartas llamado *La
Guerra Funcional* en el lenguaje de programación **OCaml**. El objetivo
es aprender las características del paradigma funcional en un lenguaje
diferente al visto en la carrera.

##La Guerra Funcional

El juego consiste en un mazo de cartas español adicionado con *cartas
especiales* cuyo comportamiento será explicado luego. El juego
comienza con 2 a 5 jugadores, que comenzarán a jugar en el orden
ingresado, a los que se les da 7 cartas para su mazo personal inical.

Una ronda típica de este juego es la siguiente: cada jugador, en
orden, tira una carta a elección de su mazo, recuperando una carta del
tope del mazo general (si hubiere). Una vez que el último de la ronda
tiró la suya, el que tenga la carta mas grande gana un punto. Gana la
carta con mayor número, y si hubiera empate ordenando por el palo
según el siguiente criterio: la espada *corta* al basto, el basto
*roba* el oro, el oro *compra* la copa.

###Cartas Especiales

Antes de jugar una carta común, un jugador puede decidir jugar una
*carta especial*, que le permite, de acuerdo a las reglas de la carta,
modificar su mazo y el mazo general. El juego cuenta con las
siguientes cartas especiales, con su correspondiente código
identificatorio:

| ID   | Descripción  |
|:----:|--------------|
| ID   | Deja los mazos intactos |
| SWAP | Cambia el mazo del jugador por el mazo general |
| MAX  | Toma la carta más alta del mazo general |
| MIN  | Toma la carta mínima del mazo del jugador y la devuelve al *fondo* del mazo general |
| TOP  | Toma una carta del tope del mazo general |
| PAR  | Toma las cartas *pares* del mazo general |

Cuando se juega una carta especial también se recupera una del tope
del mazo general. De esta forma, la carta especial ID permite tomar
una carta del tope, y TOP dos (si hubiere).

###GAME OVER

Un jugador sale del juego cuando se queda sin cartas y no hay mas
cartas en el mazo general. Sus puntos se preservan, con lo que puede
todavía ganar la partida si hizo mas puntos que el resto de los
jugadores. El juego se termina cuando queda un solo jugador (o ninguno).

##Formato

Para dar inicio al juego, se debe preguntar al usuario la carga de
cada jugador. Esto se hará mediante el siguiente texto:

    Ingrese el nombre del jugador o EXIT para comenzar el juego:

Cuando se termina la carga mediante EXIT, se comienza el juego. Para
el ejemplo vamos a suponer que se cargaron los jugadores Ta, Te, y
Ti. Se presenta la siguiente pantalla:

    Mazo: 33 cartas
    Ronda:
    Ta(0): E2 B2 C3 O2 O11 C6 STOP
    Que carta vas a jugar Ta?

La información de esta pantalla es la siguiente, línea por línea:

1. Se muestran la cantidad de cartas del mazo general, que en este
caso son 48 de la baraja clásica mas las 6 especiales, menos 21 (7
para cada jugador).

2. Se muestra la ronda, es decir, las cartas que fueron jugadas por
los jugadores (ninguna de momento).

3. Se muestra el nombre del jugador que debe jugar seguido por su
puntaje entre parentesis. En este momento juega Ta y su puntaje es
0. Luego se muestra las cartas en "la mano" de Ta.

4. Se pregunta al jugador por la carta que quiere jugar.

Las cartas comunes se presentan según su palo (**E**spada, **B**asto,
**O**ro, **C**opa) seguido por su número. Las especiales se muestran con
**S** seguido de su **ID**, y de esta manera también se
ingresan. Suponemos que el jugador no está contento con su mazo, con
lo que juega la carta TOP (ingresando STOP). Se deberá presentar la
misma pantalla, ahora con el mazo del jugador conteniendo dos nuevas
cartas tomadas del mazo general, con lo que quedan 31 cartas en éste
último. Suponemos que ahora recibe la carta E12, con lo que la
juega. Luego supongamos que Te juega O1 y Ti está por jugar C2. La
pantalla muestra:

    Mazo: 29 cartas
    Ronda:
    Ta	E12
    Te	O1
    Ti(0): E7 O11 O10 B3 O6 C3 O2
    Que carta vas a jugar Ti?

Notar el formato para mostrar la ronda. Cuando Ti juega O2, el juego
nos indica que Ta ganó la ronda

    El jugador Ta gano la ronda.

Y una nueva ronda comienza. Cuando un jugador se queda sin cartas, se
comunica antes de pasar al siguiente jugador:

    El jugador Te ya no tiene cartas.

Cuando concluye la útlima ronda, si ya no quedan mas de 1 jugador con
cartas, se debe concluir imprimiendo las posiciones. Ejemplo:

    GAME OVER. Posiciones:
    1	Ti	6
    2	Te	2
    3	Ta	1

##Consignas

1. Aunque OCaml admite el uso de estructuras mutables (punteros,
arrays, mutable records, y objetos), deben utilizar sólo su parte
funcional. Si realmente creen que es mejor utilizar punteros o arrays
en algún caso particular, pueden hacerlo si lo fundamentan bien en el
`README.md`. Pero nos guardamos el derecho a bajar nota lo mismo!

2. Como en labos anteriores, respetar la estructura de directorio
(`bin` para ejecutables, `src` para código, `Makefile`, y `README.md`).

3. Como en labos anteriores, comentar en el `README.md` cómo instalar
y ejecutar el programa, y toda decisión de diseño que consideren
importante mencionar.

##Características de la presentación

* Fecha de entrega: hasta el 06/05/2016 a las 23:59:59.999

* Deberán crear un tag indicando el release para corregir.

		git tag -a 3.0 -m 'the initial release'

* Si no está el tag no se corrige. Tampoco se consideran commits
  posteriores al tag.

##Recomendaciones

* Pueden agarrar una (parte de una) baraja española y jugar,
  utilizando los comodines como cartas especiales. Esto les
  facilitará entender el juego y los casos "borde".

* Tómense el tiempo necesario para pensar la arquitectura del programa
  y diseñar los TADs antes de empezar a codificar.

* Diseñen pensando en reutilizar el código o las abstracciones cuando
  sea posible.

* Si un fragmento de código se repite frecuentemente, abstraiganlo
  mediante una función.

* Háganse amigos de los `records`, es una funcionalidad de OCaml muy
  poderosa que les puede ahorrar trabajo.

* El módulo `Set` de OCaml es complejo de usar (requiere
  conocimientos de *functores* y en algunos casos de *módulos
  recursivos*). Tal vez les sea mas facil hacer su propio módulo, si
  lo llegaran a necesitar.

* Como el proyecto manipula un conjunto limitado de elementos (cartas
  y jugadores), no se preocupen por optimizar las estructuras de
  datos. Pero sí se evaluará si las abstracciones son adecuadas. Por
  ejemplo, si van a utilizar una lista como un conjunto, entonces
  asegúrense de que la implementación abstraiga la lista.

* `ocamlbuild` es una herramienta simple pero efectiva. Pero no tiene
  un parámetro `-o` como para decidir donde poner el ejecutable. De 
  esto se tendrán que ocupar ustedes.

* Sean consistentes con la nomenclatura. Tengan en cuenta que OCaml
  impone que los constructores y los módulos empiecen con mayúscula, y
  las funciones con minúscula.

* Documenten las funciones y las porciones de código que no son
  intuitivas.

* No abusen de los comentarios en el código.

* Si la definición de una funcionalidad es ambigua, busquen
  clarificaciones antes de codificar basados en supuestos. Esto es
  responsabilidad de ustedes.
