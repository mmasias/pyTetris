# Motivación

## ¿Por qué?

En la enseñanza de ingeniería de software se presenta un problema persistente: los estudiantes aprenden a memorizar código, pero no comprenden el proceso de pensamiento que lleva a ese código. La situación problemática típica se manifiesta cuando un alumno se enfrenta a un nuevo proyecto y pregunta: "¿Por dónde empiezo?". Esta pregunta revela que ha aprendido soluciones específicas, pero no el método general para crear soluciones.

El problema se intensifica porque la mayoría de recursos educativos presentan el resultado final sin mostrar el camino. Se enseña el "qué" (aquí está el código del Tetris) pero se omite el "cómo" (cómo se llegó a diseñar ese código) y especialmente el "por qué" (por qué se tomaron esas decisiones específicas).

Esta aproximación genera estudiantes que pueden reproducir ejemplos, pero que se paralizan ante problemas nuevos. No han desarrollado la capacidad de análisis y toma de decisiones que caracteriza a un ingeniero de software competente. La carencia se evidencia especialmente cuando deben justificar sus decisiones de diseño o cuando enfrentan múltiples alternativas de implementación.

Además, se perpetúa el mito de que el diseño de software es una habilidad mística que "algunos tienen y otros no", cuando en realidad es un proceso sistemático que puede enseñarse y aprenderse.

## ¿Qué?

pyTetris representa un enfoque pedagógico diferente: documentar completamente el proceso de desarrollo desde la conceptualización hasta la implementación, exponiendo no solo las decisiones tomadas sino también las alternativas consideradas y descartadas.

El proyecto se estructura como una progresión pedagógica de cuatro fases claramente diferenciadas:

- **Modelo del dominio**: Se capturan los conceptos puros del mundo real, sin contaminación tecnológica
- **Análisis**: Se transforman los conceptos del dominio en elementos de software
- **Decisiones tecnológicas**: Se materializan los conceptos abstractos en opciones técnicas concretas
- **Diseño**: A partir del análisis más las decisiones tecnológicas, se define la arquitectura específica, patrones y estructura de clases que implementarán las decisiones tecnológicas
- **Implementación**: Se codifica siguiendo las decisiones previas

Esta aproximación contrasta radicalmente con el enfoque tradicional de "aquí está el código, entiéndelo". En lugar de presentar la solución como un hecho consumado, se documenta el razonamiento completo que condujo a esa solución específica.

El proyecto mantiene trazabilidad completa: cada línea de código puede rastrearse hasta las decisiones tecnológicas que la motivaron, las cuales a su vez se fundamentan en el análisis conceptual del problema original.

## ¿Para qué?

Este enfoque produce múltiples beneficios educativos que trascienden el aprendizaje de un juego específico.

<div align=center>

|Para estudiantes principiantes|Para estudiantes intermedios|Para estudiantes avanzados|
|-|-|-|
|Desmitifica el proceso de desarrollo de software. Ven que el código no surge por inspiración divina, sino que es el resultado de un proceso sistemático y replicable. Comprenden que antes de programar es necesario entender el problema y tomar decisiones conscientes sobre cómo resolverlo.|Desarrolla la capacidad de análisis y toma de decisiones. Aprenden a evaluar alternativas, considerar trade-offs y justificar sus elecciones. Entienden que existen múltiples soluciones válidas para un mismo problema, cada una con ventajas y limitaciones específicas.|Proporciona un marco de referencia para la reflexión crítica sobre ingeniería de software. Pueden cuestionar si las "buenas prácticas" son aplicables en cada contexto específico y desarrollan criterio para distinguir entre elegancia teórica y pragmatismo efectivo.

</div>

El impacto más significativo radica en que los estudiantes desarrollan un método transferible. No aprenden solo a resolver el problema del Tetris, sino que adquieren un proceso que pueden aplicar a cualquier proyecto futuro. Se convierten en solucionadores de problemas, no en replicadores de soluciones existentes.

Además, se cultiva la capacidad de comunicación técnica. Los estudiantes aprenden a documentar y justificar sus decisiones, habilidad esencial para el trabajo profesional colaborativo.

## ¿Cómo?

La implementación práctica de este enfoque requiere inversión deliberada en documentación del proceso de pensamiento.

Cada fase del desarrollo se documenta exhaustivamente, no solo mostrando las decisiones tomadas sino explicando el razonamiento subyacente. Se incluyen las alternativas consideradas y los criterios utilizados para la selección final.

El proyecto utiliza diagramas UML para visualizar la progresión conceptual, permitiendo a los estudiantes ver cómo los conceptos abstractos se materializan gradualmente en estructuras de código concretas.

Se mantiene un vocabulario consistente a lo largo de todo el proceso, facilitando el seguimiento de conceptos desde su origen en el dominio hasta su implementación final.

La documentación incluye reflexiones críticas sobre las decisiones tomadas, como el análisis "valeLaPenaLaHerencia.md" que demuestra que las buenas prácticas deben aplicarse con criterio, no como dogmas inflexibles.

Se proporciona trazabilidad bidireccional: desde cada concepto del dominio hasta su implementación, y desde cada línea de código hasta las decisiones que la motivaron.

El código se estructura para facilitar la comprensión pedagógica, priorizando claridad sobre optimización. Se utilizan nombres descriptivos, se evita la complejidad innecesaria y se mantiene separación clara de responsabilidades.

## ¿Y ahora qué?

Este enfoque puede expandirse a otros dominios de problemas, creando una biblioteca de casos de estudio que demuestren cómo aplicar ingeniería de software sistemática a diferentes tipos de desafíos.

Invita a educadores a reconsiderar cómo enseñan programación y diseño de software, enfatizando el proceso de pensamiento tanto como el producto final.

Para estudiantes, proporciona un modelo replicable que pueden aplicar en sus propios proyectos, desde ejercicios académicos hasta desarrollos profesionales.

Finalmente, demuestra que la excelencia en ingeniería de software no radica en conocer todas las respuestas, sino en dominar un proceso sistemático para encontrar buenas respuestas a problemas específicos.

La verdadera medida del éxito de este enfoque no será que los estudiantes reproduzcan el código del Tetris, sino que desarrollen la capacidad de abordar problemas completamente nuevos con confianza y método.