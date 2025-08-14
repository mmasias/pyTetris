# Lenguajes de Programación Implementados

Este repositorio contiene implementaciones del juego Tetris en múltiples lenguajes de programación, todas siguiendo la misma arquitectura y diseño.

## Lenguajes Implementados

1. **[Java](src/)** (rama principal `main`)
   - Implementación original del proyecto
   - Arquitectura MVC con clases separadas para modelo, vista y controlador

2. **[Python](python_src/)**
   - Conversión directa de la implementación Java
   - Mantiene la misma estructura de clases y arquitectura

3. **[JavaScript](js-src/)**
   - Implementación en JavaScript para entorno Node.js
   - Misma arquitectura y patrones de diseño

4. **[C++](cpp_tetris/)**
   - Implementación en C++ con CMake
   - Sigue los mismos principios de diseño

5. **[Haskell](haskell-tetris/)**
   - Implementación funcional en Haskell
   - Adaptación de los conceptos a paradigma funcional

6. **[Rust](rust_tetris/)**
   - Implementación en Rust con gestión de memoria segura
   - Conversión de los patrones de diseño a conceptos de Rust

7. **[C# (.NET)](PyTetris.NET/)**
   - Implementación en C# para plataforma .NET
   - Mantiene la arquitectura MVC

## Notas

- Todas las implementaciones comparten la misma arquitectura MVC (Modelo-Vista-Controlador)
- La documentación principal se encuentra en la rama `main` (versión Java)
- La rama `traducciones` contiene todas las implementaciones adicionales en otros lenguajes
- Cada implementación mantiene los mismos principios de diseño y patrones explicados en la documentación