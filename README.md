<div align=right>

<sub>*El desarrollo de software no es un arte misterioso, sino un proceso **sistemático**<br>donde cada decisión tiene fundamento y trazabilidad<br> desde los conceptos del dominio hasta el código ejecutable.*</sub>

</div>

# pyTetris

Un Tetris ASCII

<div align=center>

![](/images/tetris.png)

</div>

## 🚬's

<div align=center>

|Observar||Conceptualizar||Decidir||Construir||Ejecutar|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|[Modelo del dominio](/docs/modeloDelDominio.md)|>>|[Análisis](/docs/ProcesoAnalisis.md)|>>|[Decisiones tecnológicas](/docs/DecisionesTecnologicas.md)|>>|[Diseño](/docs/ProcesoDiseño.md)|>>|[Código](/src/)

</div>

<div align=right>

<sub>[*Motivación*](docs/motivación.md)</sub>

</div>

## Proceso de creación

### El viaje del desarrollo: de la idea al código reflexivo

Este proyecto documenta un **proceso sistemático de desarrollo de software**, donde cada commit representa una decisión consciente en la evolución del sistema. A continuación se explica el *por qué* y *para qué* de cada paso:

#### 🚀 **Fase 1: Génesis y Implementación Inicial**

**[8e05b21]** `Initial commit`
- **¿Qué?** Inicio del repositorio con .gitignore básico
- **¿Por qué?** Establecimiento de la base del proyecto con buenas prácticas desde el inicio
- **Decisión:** Crear un espacio controlado para el desarrollo sistemático

**[79dbeff4]** `Agregar clases principales para el juego Tetris...`
- **¿Qué?** Implementación completa del núcleo funcional: `Board`, `Piece`, `Console`, `Position`, `Tetris`, `PieceFactory`
- **¿Por qué?** Demostrar que el desarrollo se puede hacer de forma iterativa pero con visión arquitectónica
- **Decisión:** Crear una versión funcional como punto de partida para el análisis posterior

**[962920b]** `README`
- **¿Qué?** Primera documentación básica del proyecto
- **¿Por qué?** Todo proyecto necesita una puerta de entrada comprensible
- **Decisión:** Documentar antes de continuar con la complejidad

#### 📐 **Fase 2: Estructuración y Documentación Visual**

**[4b6a261]** `Agregar archivo README.md y diagramas UML para el juego Tetris`
- **¿Qué?** Introducción de diagramas UML y documentación estructurada
- **¿Por qué?** *"Una imagen vale más que mil líneas de código"* - Visualizar la arquitectura
- **Decisión:** Adoptar UML como lenguaje de modelado estándar

#### 🔄 **Fase 3: Evolución Arquitectónica (Patrón MVC)**

**[d33a338]** `Refactor juego Tetris: reemplazar clase Piece por PieceView...`
- **¿Qué?** Primera transformación hacia separación de responsabilidades
- **¿Por qué?** Identificación de la necesidad de separar modelo de vista
- **Decisión:** Aplicar principios de diseño orientado a objetos

**[a3b225e]** `Refactor juego Tetris: reestructurar clases Piece y PieceView...`
- **¿Qué?** Refinamiento de la separación modelo-vista
- **¿Por qué?** Perfeccionar la implementación del patrón arquitectónico
- **Decisión:** Iterar sobre el diseño para mejorarlo

**[ed35d05]** `feat: MVC`
- **¿Qué?** Implementación completa del patrón Model-View-Controller
- **¿Por qué?** Aplicar una arquitectura probada que facilite mantenimiento y evolución
- **Decisión:** Adoptar MVC como patrón arquitectónico fundamental

#### 📋 **Fase 4: Documentación del Proceso**

**[ba6a881]** `Proceso de creación`
- **¿Qué?** Documentación de la metodología de desarrollo seguida
- **¿Por qué?** *"El desarrollo no es arte misterioso, sino proceso sistemático"*
- **Decisión:** Hacer explícito el conocimiento tácito del proceso

**[102fd16]** `Agregar análisis sobre herencia vs composición`
- **¿Qué?** Reflexión técnica sobre decisiones de diseño
- **¿Por qué?** Documentar el *por qué* de las decisiones técnicas
- **Decisión:** Crear conocimiento reutilizable para futuros proyectos

#### 🏗️ **Fase 5: Modelado del Dominio**

**[687c3d3]** `Modelo del dominio`
- **¿Qué?** Documentación formal del modelo del dominio del problema
- **¿Por qué?** Separar conceptos del mundo real de conceptos de implementación
- **Decisión:** Aplicar metodologías de ingeniería de software

**[f8d48f7]** `Ajuste en el README`
- **¿Qué?** Mejoras en la documentación principal
- **¿Por qué?** La documentación debe evolucionar con el proyecto
- **Decisión:** Mantener coherencia entre código y documentación

#### 🔍 **Fase 6: Análisis Sistemático**

**[80738910]** `Del modelo del dominio al análisis...`
- **¿Qué?** Transición formal del dominio al análisis de software
- **¿Por qué?** Aplicar una metodología estructurada de desarrollo
- **Decisión:** Seguir el proceso: Dominio → Análisis → Diseño → Código

**[507459e]** `Decisiones tecnológicas`
- **¿Qué?** Documentación explícita de decisiones tecnológicas
- **¿Por qué?** Cada elección técnica debe ser justificada y documentada
- **Decisión:** Crear trazabilidad entre decisiones y implementación

**[14c21a5]** `fix numeración`
- **¿Qué?** Corrección de formato en documentación
- **¿Por qué?** La calidad incluye los detalles de presentación
- **Decisión:** Mantener estándares de calidad en toda la documentación

#### 🎯 **Fase 7: Diseño Detallado**

**[75c866b]** `Motivación (bien entendida, no las m... que dicen que son motivación)`
- **¿Qué?** Documentación de la motivación real del proyecto
- **¿Por qué?** Distinguir entre motivación genuina y motivación superficial
- **Decisión:** Ser honesto sobre las razones del desarrollo

**[550acd6]** `Merge pull request #1 from mmasias/PatronVistaModelo`
- **¿Qué?** Integración formal de cambios mediante pull request
- **¿Por qué?** Aplicar buenas prácticas de desarrollo colaborativo
- **Decisión:** Usar herramientas de control de versiones profesionalmente

**[bd7fe48]** `Acerca del diseño...`
- **¿Qué?** Documentación del proceso de diseño
- **¿Por qué?** El diseño merece tanta atención como el código
- **Decisión:** Documentar el proceso de toma de decisiones de diseño

**[9ad61630]** `ProcesoDeDiseño++ y versión 001 :)`
- **¿Qué?** Expansión de la documentación de diseño y marcado de versión
- **¿Por qué?** Celebrar hitos y documentar evolución
- **Decisión:** Reconocer logros en el proceso de desarrollo

#### 📚 **Fase 8: Refinamiento y Organización**

**[bfdd4de]** `Modelado del documento de MdD siguiendo el patrón de Análisis & Diseño`
- **¿Qué?** Reestructuración de documentos siguiendo metodología propia
- **¿Por qué?** Aplicar patrones de documentación consistentes
- **Decisión:** Crear coherencia metodológica en toda la documentación

**[2e335c69]** `refactor: documentos`
- **¿Qué?** Refactorización general de la documentación
- **¿Por qué?** La documentación también necesita refactoring como el código
- **Decisión:** Tratar la documentación con el mismo rigor que el código

**[d8e8f766]** `refactor: actualizar tablas de navegación en documentos`
- **¿Qué?** Mejora de la navegabilidad entre documentos
- **¿Por qué?** Facilitar la comprensión del proceso completo
- **Decisión:** Crear una experiencia de usuario fluida en la documentación

**[4c6a5be]** `refactor: mejorar referencias y claridad en documentos de análisis y diseño`
- **¿Qué?** Perfeccionamiento de enlaces y claridad
- **¿Por qué?** La documentación debe ser autoexplicativa y navegable
- **Decisión:** Optimizar la experiencia de lectura y comprensión

#### 🤔 **Fase 9: Reflexión Final**

**[e468fa6]** `Reflexión`
- **¿Qué?** Commit final que cierra el ciclo de desarrollo documentado
- **¿Por qué?** Todo proceso de aprendizaje requiere reflexión final
- **Decisión:** Cerrar el ciclo con una mirada retrospectiva al proceso completo

### 💡 **Lecciones del Proceso**

Este desarrollo demuestra que:

1. **El código es solo el 20% del desarrollo** - El 80% restante es análisis, diseño, documentación y reflexión
2. **Cada commit debe tener propósito** - No hay cambios aleatorios, solo evolución sistemática
3. **La documentación evoluciona** - Como el código, requiere refactoring y mejora continua
4. **El proceso es tan importante como el producto** - Documentar el *cómo* y *por qué* es tan valioso como el *qué*
5. **La reflexión cierra el ciclo** - Todo proceso de desarrollo debe incluir aprendizaje explícito

---

## 2Think

- [¿Vale la pena la herencia?](docs/valeLaPenaLaHerencia.md)
