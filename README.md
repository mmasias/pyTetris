<div align=right>

<sub>*El desarrollo de software no es un arte misterioso, sino un proceso **sistemático**<br>donde cada decisión tiene fundamento y trazabilidad<br> desde los conceptos del dominio hasta el código ejecutable.*</sub>

</div>

# pyTetris

Un Tetris ASCII disponible en Java y Python

<div align=center>

![](/images/tetris.png)

</div>

## Versiones Disponibles

### Java (Original)
- **Ubicación:** `/src/`
- **Compilación:** `javac src/*.java`
- **Ejecución:** `java -cp src Game`

### Python (Conversión)
- **Ubicación:** `/python_src/`
- **Instalación:** `pip install -r requirements.txt`
- **Ejecución:** `python -m python_src.game`
- **Tests:** `python -m pytest tests/ -v`

## 🚬's

<div align=center>

|Observar||Conceptualizar||Decidir||Construir||Ejecutar|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|[Modelo del dominio](/docs/modeloDelDominio.md)|>>|[Análisis](/docs/ProcesoAnalisis.md)|>>|[Decisiones tecnológicas](/docs/DecisionesTecnologicas.md)|>>|[Diseño](/docs/ProcesoDiseño.md)|>>|[Código](/src/)

</div>

## Conversión Java → Python

📋 **[Documentación completa de la conversión](/docs/CONVERSION.md)**

La conversión mantiene toda la funcionalidad original mientras implementa buenas prácticas de Python:
- ✅ Arquitectura orientada a objetos preservada
- ✅ 29 tests unitarios con pytest
- ✅ Type hints y documentación completa  
- ✅ Gestión de dependencias con pyproject.toml
- ✅ Compatibilidad multiplataforma mejorada

<div align=right>

<sub>[*Motivación*](docs/motivación.md)</sub>

</div>

## 2Think

- [¿Vale la pena la herencia?](docs/valeLaPenaLaHerencia.md)
