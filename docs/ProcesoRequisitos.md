<div align=center>

|Observar||Conceptualizar||Decidir||Construir||Ejecutar|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|[Modelo del dominio](/docs/modeloDelDominio.md)|>>|[***Requisitos***](/docs/ProcesoRequisitos.md)|>>|[Análisis](/docs/ProcesoAnalisis.md)|>>|[Decisiones tecnológicas](/docs/DecisionesTecnologicas.md)|>>|[Diseño](/docs/ProcesoDiseño.md)|>>|[Código](/src/)

</div>

# Proceso de Requisitos: Tetris

Los requisitos especifican QUÉ debe hacer el sistema desde la perspectiva del usuario, sin contaminación de decisiones de implementación. Transforman los conceptos del dominio en comportamientos concretos del software.

## Metodología

### Punto de partida: modelo del dominio

Los requisitos parten del modelo del dominio que identificó conceptos del mundo real:

- Juego, Tablero, Pieza, TipoPieza
- Celda, Línea, Movimiento, Turno

**Pregunta central**: ¿Qué solicitudes puede realizar un actor externo al sistema utilizando estos conceptos?

### Actores

**Técnica**: Análisis de entidades externas que interactúan con el sistema

|Actor|Descripción|Justificación|
|-|-|-|
|Jugador|Persona que controla el juego|Solicita manipulación de piezas y gestión de partida|
|Tiempo|Disparador temporal|Solicita descenso automático de piezas según intervalo configurado|

### Casos de Uso

**Técnica**: Análisis de objetivos que cada actor busca alcanzar

Del modelo del dominio surge:

| Comportamiento Identificado|¿Quién lo Solicita?
|-|:-:|
| Iniciar nueva partida|Jugador |
|Mover pieza lateralmente|Jugador
|Rotar pieza|Jugador
|Acelerar caída de pieza|Jugador
|Descender pieza automáticamente|Tiempo

### Presentar el modelo de casos de uso como un todo

#### Estados del sistema

<div align=center>

|![](/images/modelosUML/DiagramaDeContexto000.svg)
|-

</div>

#### Diagramas de contexto

<div align=center>

|Jugador|Tiempo|
|-|-|
|![](/images/modelosUML/DiagramaDeContexto001-ActorJugador.svg)|![](/images/modelosUML/DiagramaDeContexto001-ActorTiempo.svg)

</div>
