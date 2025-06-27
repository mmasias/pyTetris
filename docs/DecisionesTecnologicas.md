<div align=center>

|Observar||Conceptualizar||Decidir||Construir||Ejecutar|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|[Modelo del dominio](/docs/modeloDelDominio.md)|>>|[Análisis](/docs/ProcesoAnalisis.md)|>>|[***Decisiones tecnológicas***](/docs/DecisionesTecnologicas.md)|>>|[Diseño](/docs/ProcesoDiseño.md)|>>|[Código](/src/)

</div>

# Decisiones tecnológicas: del análisis al diseño

## ¿Qué son las decisiones tecnológicas?

Las **decisiones tecnológicas** son el puente entre el **QUÉ** (análisis) y el **CÓMO** (diseño). Aquí decidimos:

- Qué tecnologías usar
- Cómo representar los conceptos abstractos
- Qué patrones de diseño aplicar
- Cómo estructurar la arquitectura

## Contexto del proyecto

### Requisitos no funcionales identificados

- **Plataforma:** Aplicación de escritorio
- **Interfaz:** Terminal/Consola (simplicidad)
- **Audiencia:** Educativa/demostrativa
- **Complejidad:** Moderada (enfoque didáctico)
- **Rendimiento:** No crítico
- **Mantenibilidad:** Alta (código educativo)

## Decisiones tecnológicas tomadas

### 1. Lenguaje de programación: Java

**Concepto de análisis afectado:** Todos

**Decisión:** Java como lenguaje principal

**Alternativas consideradas:**

- Python (simplicidad)
- C++ (rendimiento)
- JavaScript (web)

**Justificación:**

- **Orientado a objetos:** Mapeo natural de conceptos del análisis
- **Tipado fuerte:** Detección temprana de errores
- **Didáctico:** Excelente para enseñar OOP
- **Multiplataforma:** Disponible en múltiples sistemas
- **Estructura clara:** Fuerza buenas prácticas de diseño

**Impacto en el diseño:**

- Cada concepto del análisis se convierte en una clase Java
- Necesidad de definir tipos explícitos (`int`, `boolean[][]`, etc.)

### 2. Interfaz de usuario: consola de texto

**Concepto de análisis afectado:** `VisualizadorJuego`, `ControladorEntrada`

**Decisión:** Interfaz de terminal con caracteres ASCII

**Alternativas consideradas:**

- GUI con Swing/JavaFX
- Interfaz web
- Aplicación móvil

**Justificación:**

<div align=center>

|||
|-|-|
|**Simplicidad:** Enfoque en la lógica, no en la UI|**xElUsuario:** Porque el usuario lo quiere así
|**Universalidad:** Funciona en cualquier terminal||
|**Didáctico:** Sin distracciones visuales complejas||
|**Rapidez de desarrollo:** Menos código de interfaz||

</div>

**Impacto en el diseño:**

- `VisualizadorJuego` → `BoardView` + `PieceView` + `Console`
- `ControladorEntrada` → métodos en `Tetris` + `Console.readString()`
- Necesidad de una clase `Console` para I/O

### 3. Representación del tablero: array 2D de caracteres

**Concepto de análisis afectado:** `Tablero`

**Decisión:** `char[][]` para representar el estado del tablero

**Alternativas consideradas:**

- `int[][]` con códigos numéricos
- `boolean[][]` con tabla separada de símbolos
- Estructura de objetos `Cell`

**Justificación:**

- **Eficiencia:** Acceso directo O(1)
- **Simplicidad:** Mapeo directo carácter-pantalla
- **Memoria:** Representación compacta
- **Visualización:** Fácil de renderizar

**Impacto en el diseño:**

- `Tablero.estado` → `Board.grid : char[][]`
- Métodos específicos para manipular arrays
- Necesidad de coordenadas (x,y) explícitas

### 4. Representación de piezas: array 2D de booleanos

**Concepto de análisis afectado:** `Pieza`, `TipoPieza`

**Decisión:** `boolean[][]` para las formas de las piezas

**Alternativas consideradas:**

- Coordenadas relativas `List<Position>`
- Bitmask con operaciones bit
- Enumeración de formas predefinidas

**Justificación:**

- **Intuitividad:** Representa visualmente la forma
- **Rotación:** Algoritmos de rotación claros
- **Colisión:** Detección simple con loops anidados
- **Extensibilidad:** Fácil agregar nuevas formas

**Impacto en el diseño:**

- `TipoPieza` → métodos estáticos en `PieceFactory`
- `Pieza.forma` → `Piece.shape : boolean[][]`
- Algoritmos específicos de rotación de matrices

### 5. Patrón de creación: factory

**Concepto de análisis afectado:** `GeneradorPiezas`

**Decisión:** Clase `PieceFactory` con métodos estáticos

**Alternativas consideradas:**

- Constructor directo de `Piece`
- Builder pattern
- Abstract Factory

**Justificación:**

- **Encapsulación:** Oculta la complejidad de creación
- **Consistencia:** Garantiza piezas válidas
- **Mantenibilidad:** Cambios centralizados
- **Simplicidad:** No requiere herencia compleja

**Impacto en el diseño:**

- `GeneradorPiezas` → `PieceFactory`
- Métodos como `createIPiece()`, `createOPiece()`
- Creación paralela de `Piece` y `PieceView`

### 6. Arquitectura: MVC (Modelo-Vista-Controlador)

**Concepto de análisis afectado:** Toda la estructura

**Decisión:** Separación clara en tres capas

**Alternativas consideradas:**

- Arquitectura monolítica
- MVP (Modelo-Vista-Presentador)
- Arquitectura hexagonal

**Justificación:**

- **Separación de responsabilidades:** Cada capa tiene un propósito claro
- **Testabilidad:** Modelo independiente de la vista
- **Mantenibilidad:** Cambios aislados por capa
- **Didáctico:** Patrón arquitectónico fundamental

**Impacto en el diseño:**

- **Modelo:** `Board`, `Piece`, `Position`, `PieceFactory`
- **Vista:** `BoardView`, `PieceView`, `Console`
- **Controlador:** `Tetris`, `Game`

### 7. Gestión de entrada: bloqueo síncrono

**Concepto de análisis afectado:** `ControladorEntrada`

**Decisión:** `BufferedReader` con lectura síncrona

**Alternativas consideradas:**

- Input asíncrono con threads
- Listeners de eventos
- Polling de teclas

**Justificación:**

- **Simplicidad:** Un comando por turno
- **Estabilidad:** Sin condiciones de carrera
- **Didáctico:** Flujo de control claro
- **Multiplataforma:** Funciona en cualquier terminal

**Impacto en el diseño:**

- Game loop en `Tetris.gameLoop()`
- `Console.readString()` para captura
- Procesamiento inmediato en `processUserInput()`

### 8. Manejo de colores: ANSI escape codes

**Concepto de análisis afectado:** `VisualizadorJuego`

**Decisión:** Códigos ANSI para colores en terminal

**Alternativas consideradas:**

- Solo texto monocromático
- Librerías de terminal avanzadas
- Interfaz gráfica

**Justificación:**

- **Compatibilidad:** Soportado por la mayoría de terminales
- **Simplicidad:** No requiere librerías externas
- **Funcionalidad:** Suficiente para la demo
- **Graceful degradation:** Funciona sin colores

**Impacto en el diseño:**

- Enums `ForegroundColor`, `BackgroundColor` en `Console`
- Métodos sobrecargados para escritura con colores
- Detección automática de soporte de colores

## Matriz de trazabilidad: Análisis → Decisiones → Diseño

<div align=center>

|Concepto análisis|Decisión tecnológica|Resultado en diseño|
|-|-|-|
|`Juego`|Clase controladora principal|`Tetris` (controlador) + `Game` (main)|
|`Tablero`|Array 2D de caracteres|`Board` con `char[][] grid`|
|`Pieza`|Array 2D de booleanos + Posición|`Piece` con `boolean[][] shape` + `Position`|
|`TipoPieza`|Factory Method|`PieceFactory` con métodos estáticos|
|`GeneradorPiezas`|Métodos estáticos + Random|`PieceFactory` + `Math.random()`|
|`VisualizadorJuego`|MVC Vista|`BoardView` + `PieceView` + `Console`|
|`ControladorEntrada`|Lectura síncrona|`Console.readString()` + `processUserInput()`|
|`Posicion`|Coordenadas enteras|`Position` con `int x, y`|

</div>

## Compromisos

<div align=center>

|Beneficios de las decisiones|Limitaciones aceptadas|
|-|-|
|**Simplicidad:** Código fácil de entender y mantener|**Rendimiento:** No optimizado para juegos de alta velocidad|
|**Portabilidad:** Ejecuta en cualquier JVM|**UI:** Interfaz básica, no moderna|
|**Didáctico:** Excelente para enseñar conceptos OOP|**Concurrencia:** No hay threading para animaciones fluidas|
|**Eficiencia:** Representaciones de datos apropiadas|**Escalabilidad:** No preparado para features complejas|

</div>

## Conclusión

Estas decisiones tecnológicas transforman los conceptos abstractos del análisis en elementos concretos implementables, manteniendo la **trazabilidad** entre lo que queremos hacer (análisis) y cómo lo vamos a hacer (diseño).

El siguiente paso será tomar estas decisiones y materializarlas en el código final durante la fase de diseño detallado.
