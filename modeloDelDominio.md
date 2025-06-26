# Modelo del dominio

<div align=center>

![](/images/modelosUML/ModeloDominio.svg)

</div>

## Glosario

> *Porque a algunos les hace ilusión...*

<div align=center>

|||
|-|-|
Juego|Concepto central del dominio. Representa una partida de Tetris con sus reglas y estado.
|Tablero|Área conceptual donde caen y se acumulan las piezas del juego.
|Pieza|Elementos geométricos que caen y que el jugador puede manipular.|
|TipoPieza|Clasificación de las piezas según su forma característica(I, O, T, L, S, Z, J).
|Celda|Unidad mínima del espacio de juego. Puede estar vacía o ocupada por una pieza. 
|Linea|Fila horizontal del tablero. Se elimina cuando está completamente llena. 
|Movimiento|Acción que puede realizar el jugador: mover lateralmente, rotar, o acelerar caída. 
|Turno|Unidad temporal del juego. Desde que aparece una pieza hasta que se coloca.

</div>