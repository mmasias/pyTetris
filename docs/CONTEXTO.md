# Contexto del Proyecto - Lenguajes de Programación

Este documento contiene información importante sobre las diferentes implementaciones
del proyecto pyTetris en múltiples lenguajes de programación.

## Información del Repositorio

- **Ubicación**: `/home/manuel/misRepos/pyTetris`
- **Rama principal**: `main` (implementación en Java)
- **Rama con implementaciones adicionales**: `traducciones`

## Lenguajes Implementados

1. **Java** (rama principal `main`)
   - Implementación original del proyecto
   - Arquitectura MVC con clases separadas para modelo, vista y controlador
   - Directorio: `src/`

2. **Python** (en rama `traducciones`)
   - Conversión directa de la implementación Java
   - Mantiene la misma estructura de clases y arquitectura
   - Directorio: `python_src/`

3. **JavaScript** (en rama `traducciones`)
   - Implementación en JavaScript para entorno Node.js
   - Misma arquitectura y patrones de diseño
   - Directorio: `js-src/`

4. **C++** (en rama `traducciones`)
   - Implementación en C++ con CMake
   - Sigue los mismos principios de diseño
   - Directorio: `cpp_tetris/`

5. **Haskell** (en rama `traducciones`)
   - Implementación funcional en Haskell
   - Adaptación de los conceptos a paradigma funcional
   - Directorio: `haskell-tetris/`

6. **Rust** (en rama `traducciones`)
   - Implementación en Rust con gestión de memoria segura
   - Conversión de los patrones de diseño a conceptos de Rust
   - Directorio: `rust_tetris/`

7. **C# (.NET)** (en rama `traducciones`)
   - Implementación en C# para plataforma .NET
   - Mantiene la arquitectura MVC
   - Directorio: `PyTetris.NET/`

## Instrucciones para Navegar entre Implementaciones

Para acceder a las implementaciones en otros lenguajes:

```bash
# Cambiar a la rama con todas las implementaciones
git checkout traducciones

# Listar todas las implementaciones disponibles
ls -la

# Volver a la implementación principal en Java
git checkout main
```

## Notas Importantes

- Todas las implementaciones comparten la misma arquitectura MVC (Modelo-Vista-Controlador)
- La documentación principal se encuentra en la rama `main` (versión Java)
- Cada implementación mantiene los mismos principios de diseño y patrones explicados en la documentación
- Este proyecto tiene un enfoque pedagógico fuerte, documentando todo el proceso de desarrollo