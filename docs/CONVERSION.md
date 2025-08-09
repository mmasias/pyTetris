# Conversión de Código Java a Python - PyTetris

## Resumen

Este documento detalla el proceso de conversión del código Java de Tetris a Python, incluyendo las decisiones técnicas, compromisos y buenas prácticas implementadas.

## Estructura Original (Java)

El proyecto original en Java consistía en 9 clases:

- **Game.java** - Punto de entrada principal
- **Tetris.java** - Controlador principal del juego con lógica de bucle
- **Board.java** - Gestión del tablero y estado del juego
- **Piece.java** - Lógica de piezas individuales (movimiento, rotación)
- **Position.java** - Clase utilitaria para coordenadas 2D
- **PieceFactory.java** - Factory para crear diferentes tipos de piezas
- **BoardView.java** - Renderizado del tablero en consola
- **PieceView.java** - Renderizado de piezas individuales
- **Console.java** - Utilidad compleja de I/O con soporte de colores ANSI

## Estructura Nueva (Python)

La conversión mantiene la misma arquitectura pero adaptada a las convenciones de Python:

```
python_src/
├── __init__.py
├── game.py              # Punto de entrada (Game.java)
├── tetris.py            # Controlador principal (Tetris.java)
├── board.py             # Gestión del tablero (Board.java)
├── piece.py             # Lógica de piezas (Piece.java)
├── position.py          # Coordenadas 2D (Position.java)
├── piece_factory.py     # Factory de piezas (PieceFactory.java)
├── board_view.py        # Vista del tablero (BoardView.java)
├── piece_view.py        # Vista de piezas (PieceView.java)
└── console.py           # I/O simplificado (Console.java)

tests/
├── __init__.py
├── test_position.py
├── test_piece.py
├── test_piece_factory.py
└── test_board.py
```

## Decisiones Técnicas

### 1. Convenciones de Nomenclatura

**Java → Python**
- `camelCase` → `snake_case`
- `getX()` / `setX()` → propiedades o acceso directo
- Nombres de archivos: `PascalCase.java` → `snake_case.py`

**Ejemplo:**
```java
// Java
public int getWidth() { return width; }
public void moveDown() { position.setY(position.getY() + 1); }
```

```python
# Python
def get_width(self) -> int:
    return self.width

def move_down(self) -> None:
    self.position.y += 1
```

### 2. Gestión de Tipos

**Java (tipado estático):**
```java
private char[][] grid;
private boolean[][] shape;
```

**Python (tipado con hints):**
```python
from typing import List, Optional

self.grid: List[List[str]] = []
shape: List[List[bool]]
```

### 3. Simplificación de la Clase Console

**Decisión:** Simplificar la clase Console compleja de Java usando bibliotecas de Python estándar.

**Java:** 349 líneas con enums anidados, múltiples sobrecargas
**Python:** 180 líneas con enum.Enum y colorama para compatibilidad multiplataforma

**Compromiso:** Se perdieron algunas funcionalidades avanzadas de colores, pero se ganó simplicidad y compatibilidad.

```java
// Java - Múltiples sobrecargas
public void write(String string, ForegroundColor color) { ... }
public void write(int value, ForegroundColor color) { ... }
public void write(double value, ForegroundColor color) { ... }
```

```python
# Python - Un método unificado con Union types
def write(self, value: Union[str, int, float, bool], 
          fg_color: ForegroundColor = None,
          bg_color: BackgroundColor = None) -> None:
```

### 4. Manejo de Arrays/Listas

**Java:**
```java
boolean[][] shape = new boolean[cols][rows];
for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
        rotated[j][rows - 1 - i] = shape[i][j];
    }
}
```

**Python:**
```python
rotated = [[False] * rows for _ in range(cols)]
for i in range(rows):
    for j in range(cols):
        rotated[j][rows - 1 - i] = shape[i][j]
```

### 5. Gestión de Memoria

**Java:** Garbage collection automático, pero requiere manejo explícito de arrays
**Python:** Gestión automática de memoria, listas dinámicas más flexibles

## Cuestiones y Compromisos

### 1. Performance
**Compromiso:** Python es interpretado vs Java compilado
**Mitigación:** Para un juego como Tetris, la diferencia es imperceptible

### 2. Tipos de Datos
**Decisión:** Usar `List[List[bool]]` en lugar de arrays nativos
**Razón:** Mayor flexibilidad y legibilidad en Python

### 3. Manejo de Colores
**Decisión:** Usar colorama para compatibilidad multiplataforma
**Compromiso:** Dependencia externa adicional vs compatibilidad mejorada

### 4. Estructura de Paquetes
**Decisión:** Mantener clases separadas en lugar de usar módulos más grandes
**Razón:** Preservar la arquitectura original para facilitar comparación

## Implementación de Buenas Prácticas

### 1. Gestión de Dependencias
```toml
# pyproject.toml
[project]
dependencies = ["colorama>=0.4.6"]

[project.scripts]
pytetris = "python_src.game:main"
```

### 2. Testing Exhaustivo
- **29 tests unitarios** cubriendo todas las clases
- **pytest** como framework de testing
- **Cobertura de código** con pytest-cov
- **Tests de rotación, colisión, líneas completas**

### 3. Documentación
- **Docstrings** en formato Google/Sphinx
- **Type hints** completas
- **Comentarios explicativos** donde necesario

### 4. Estructura de Proyecto
```
pyTetris/
├── python_src/          # Código fuente
├── tests/              # Tests unitarios
├── requirements.txt    # Dependencias de producción
├── requirements-dev.txt # Dependencias de desarrollo
├── pyproject.toml      # Configuración del proyecto
└── .gitignore          # Archivos ignorados (Python + Java)
```

## Validación de la Conversión

### Tests Ejecutados
```bash
$ python -m pytest tests/ -v
================================================= 29 passed in 0.05s =================================================
```

### Funcionalidad Conservada
✅ Mismo algoritmo de rotación de piezas
✅ Misma lógica de detección de colisiones  
✅ Mismo sistema de puntuación (100 puntos por línea)
✅ Mismos controles (4=izq, 6=der, 7=rotar↺, 9=rotar↻)
✅ Misma interfaz ASCII con bordes
✅ Mismos tipos de piezas (I, O, T, L)

## Comandos para Ejecutar

### Instalación
```bash
pip install -r requirements.txt
pip install -r requirements-dev.txt  # Para desarrollo
```

### Ejecución
```bash
python -m python_src.game
# o
python python_src/game.py
```

### Testing
```bash
python -m pytest tests/ -v --cov=python_src
```

## Conclusiones

La conversión de Java a Python se completó exitosamente manteniendo:

1. **Funcionalidad completa** del juego original
2. **Arquitectura orientada a objetos** similar
3. **Legibilidad mejorada** aprovechando características de Python
4. **Testing robusto** con cobertura completa
5. **Buenas prácticas** de desarrollo Python

Los compromisos realizados (simplificación de Console, uso de listas dinámicas) resultaron en código más limpio y mantenible sin pérdida significativa de funcionalidad.