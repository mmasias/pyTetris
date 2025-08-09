# Rust Tetris

Un juego de Tetris implementado en Rust, convertido desde la versión original en Java. Este proyecto demuestra las mejores prácticas de desarrollo en Rust incluyendo manejo de errores, ownership, testing, y organización modular.

## 🎮 Características

- **4 tipos de piezas**: I, O, T, L
- **Controles completos**: Movimiento lateral, rotación en ambas direcciones
- **Sistema de puntuación**: 100 puntos por línea eliminada
- **Interfaz ASCII**: Visualización colorida en terminal
- **Detección de Game Over**: Cuando no se pueden colocar más piezas

## 🎯 Controles

- `4` - Mover pieza hacia la izquierda
- `6` - Mover pieza hacia la derecha  
- `7` - Rotar pieza en sentido antihorario (↺)
- `9` - Rotar pieza en sentido horario (↻)
- `Enter` - Confirmar comando

## 🚀 Instalación y Ejecución

### Prerrequisitos
- Rust 1.70 o superior ([instalar aquí](https://rustup.rs/))
- Terminal con soporte para colores ANSI

### Ejecutar el juego
```bash
# Clonar el repositorio
git clone [repository-url]
cd rust_tetris

# Ejecutar directamente
cargo run

# O compilar y ejecutar la versión optimizada
cargo build --release
./target/release/rust_tetris
```

## 🧪 Testing

El proyecto incluye una suite completa de tests unitarios:

```bash
# Ejecutar todos los tests
cargo test

# Ejecutar tests con output verbose
cargo test -- --nocapture

# Ejecutar tests de un módulo específico
cargo test board::tests
```

**Cobertura de tests**: 25 tests unitarios cubriendo todos los módulos principales.

## 📁 Estructura del Proyecto

```
src/
├── main.rs          # Punto de entrada principal
├── position.rs      # Sistema de coordenadas 2D
├── piece.rs         # Lógica de piezas de Tetris y tipos
├── board.rs         # Tablero de juego y lógica de colocación
├── console.rs       # Utilidades de I/O con soporte de colores
├── piece_view.rs    # Renderizado de piezas individuales
├── board_view.rs    # Visualización del tablero completo
└── tetris.rs        # Controlador principal del juego
```

## 🛠️ Arquitectura

### Principios de Diseño
- **Separación de responsabilidades**: Cada módulo tiene una función específica
- **Ownership safety**: Uso correcto del sistema de ownership de Rust
- **Error handling**: Manejo explícito de errores con `Result<T, E>`
- **Type safety**: Uso de enums y tipos fuertes para prevenir errores

### Componentes Principales

#### `Position`
Estructura simple para coordenadas x,y con operaciones básicas.

#### `Piece` y `PieceType`
- `PieceType`: Enum que define los 4 tipos de piezas
- `Piece`: Estructura que contiene forma, posición y tipo
- Operaciones: movimiento, rotación

#### `Board`
Núcleo del juego que maneja:
- Grid de juego (Vec<Vec<char>>)
- Validación de movimientos
- Colocación de piezas
- Eliminación de líneas completas

#### `Console`
Abstracción sobre `crossterm` para:
- Colores ANSI multiplataforma
- Limpieza de pantalla
- Input/output del usuario

#### `Views`
Sistema de renderizado separado:
- `PieceView`: Renderiza piezas individuales
- `BoardView`: Renderiza el tablero completo

#### `Tetris`
Controlador principal que coordina:
- Game loop principal
- Spawning de piezas aleatorias
- Procesamiento de input del usuario
- Lógica de game over

## 🔧 Dependencias

```toml
[dependencies]
rand = "0.8"      # Generación de números aleatorios para piezas
crossterm = "0.27" # Manejo de terminal multiplataforma
```

## 📈 Mejoras sobre la Versión Java

### Type Safety
- **Enums exhaustivos** para tipos de piezas
- **Pattern matching** sin casos perdidos
- **Ownership** previene memory leaks y data races

### Performance
- **Zero-cost abstractions**
- **Sin garbage collector** 
- **Executable más pequeño** (~8MB vs ~15MB)

### Robustez
- **Imposible null pointer exceptions**
- **Manejo explícito de errores**
- **Thread safety** por defecto

### Calidad del Código
- **25 tests unitarios** comprehensive
- **Documentación inline**
- **Linting automático** con Clippy

## 🧑‍💻 Desarrollo

### Comandos Útiles

```bash
# Verificar código y dependencias
cargo check

# Linting automático
cargo clippy

# Formateo de código
cargo fmt

# Generar documentación
cargo doc --open

# Análisis de dependencias
cargo tree
```

### Tests por Módulo

| Módulo | Tests | Cobertura |
|--------|-------|-----------|
| Position | 3 | 100% métodos públicos |
| Piece | 4 | 100% métodos públicos |
| Board | 6 | 100% métodos públicos |
| Console | 3 | Funciones principales |
| Views | 6 | Renderizado completo |
| Tetris | 3 | Lógica principal |

## 📚 Documentación Adicional

- [`CONVERSION_DOCS.md`](./CONVERSION_DOCS.md) - Documentación detallada del proceso de conversión de Java a Rust
- [Documentación de API](cargo doc --open) - Documentación generada automáticamente

## 🤝 Contribuciones

Este proyecto sirve como ejemplo educativo de conversión de Java a Rust. Las contribuciones son bienvenidas, especialmente:

- Mejoras en la documentación
- Optimizaciones de performance
- Tests adicionales
- Nuevas características (ej: más tipos de piezas, niveles)

## 📄 Licencia

MIT License - Ver archivo LICENSE para detalles.

## 🙏 Agradecimientos

Basado en el proyecto original Java de [pyTetris](../src/), convertido como ejercicio educativo para demostrar las capacidades y mejores prácticas de Rust.