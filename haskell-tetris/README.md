# Tetris en Haskell

Una conversión funcional del juego Tetris desde Java a Haskell, demostrando principios de programación funcional y estructuras de datos inmutables.

## Características

- ✅ Juego Tetris completamente funcional
- ✅ 4 tipos de piezas (I, O, T, L)
- ✅ Rotación y movimiento de piezas
- ✅ Detección de colisiones
- ✅ Eliminación de líneas completas
- ✅ Sistema de puntuación
- ✅ Interfaz ASCII colorida
- ✅ Testing completo con HSpec

## Controles

- `4` - Mover izquierda
- `6` - Mover derecha  
- `7` - Rotar sentido antihorario
- `9` - Rotar sentido horario
- `q` - Salir del juego

## Instalación y Ejecución

### Prerrequisitos

- GHC (Glasgow Haskell Compiler) 9.0+
- Cabal 3.0+

### Instalación

```bash
# Clonar el repositorio
git clone <repository-url>
cd pyTetris/haskell-tetris

# Actualizar índice de paquetes
cabal update

# Compilar el proyecto
cabal build

# Ejecutar el juego
cabal run tetris
```

### Testing

```bash
# Ejecutar todos los tests
cabal test

# Ejecutar tests con salida detallada
cabal test --test-show-details=always
```

## Arquitectura

### Módulos Principales

- **Tetris.Types**: Tipos de datos principales (Position, Piece, Board, GameState)
- **Tetris.Pieces**: Operaciones con piezas (creación, movimiento, rotación)
- **Tetris.Board**: Lógica del tablero (colocación, detección de colisiones, eliminación de líneas)
- **Tetris.Game**: Bucle principal del juego y lógica de control
- **Tetris.Console**: Entrada/salida de consola con soporte de colores
- **Tetris.Utils**: Utilidades generales

### Principios de Diseño

1. **Inmutabilidad**: Todas las estructuras de datos son inmutables
2. **Pureza Funcional**: Separación entre lógica pura e efectos de IO
3. **Tipos Seguros**: Uso extensivo del sistema de tipos para prevenir errores
4. **Composabilidad**: Funciones pequeñas y composables

## Ejemplo de Uso

```haskell
import Tetris.Game

main :: IO ()
main = startStandardGame  -- Inicia un juego 10x20
```

## Testing

El proyecto incluye una suite completa de tests:

```bash
30 examples, 0 failures
```

### Cobertura de Tests

- ✅ Operaciones con posiciones
- ✅ Creación y manipulación de piezas
- ✅ Lógica del tablero
- ✅ Cálculo de puntuación
- ✅ Estados del juego

## Comparación con la Versión Java

| Aspecto | Java | Haskell |
|---------|------|---------|
| Líneas de código | ~800 | ~600 |
| Archivos | 9 | 6 módulos |
| Mutabilidad | Estado mutable | Inmutable |
| Manejo de errores | Excepciones | Maybe/Either |
| Testing | Manual | 30 tests automatizados |
| Seguridad de tipos | Básica | Avanzada |

## Documentación Técnica

Consulta [CONVERSION_DOCUMENTATION.md](CONVERSION_DOCUMENTATION.md) para detalles sobre:

- Proceso de conversión paso a paso
- Decisiones arquitectónicas
- Desafíos técnicos y soluciones
- Beneficios de la aproximación funcional

## Contribuir

1. Fork el proyecto
2. Crea una rama para tu feature (`git checkout -b feature/AmazingFeature`)
3. Commit tus cambios (`git commit -m 'Add some AmazingFeature'`)
4. Push a la rama (`git push origin feature/AmazingFeature`)
5. Abre un Pull Request

## Licencia

Este proyecto está bajo la Licencia MIT - ver el archivo [LICENSE](LICENSE) para detalles.

## Reconocimientos

- Basado en la versión original en Java del repositorio pyTetris
- Inspirado en principios de programación funcional
- Utiliza las librerías `ansi-terminal`, `random`, y `hspec`