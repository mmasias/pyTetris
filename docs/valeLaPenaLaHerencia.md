# Herencia vs Composición: el caso de las piezas del Tetris

*Un análisis práctico acerca de la sobreingeniería*

## ¿Por qué?

En el desarrollo de software se presenta frecuentemente una situación problemática: la tentación de aplicar patrones de diseño "porque son buenas prácticas", sin evaluar si realmente resuelven un problema específico. En el contexto del proyecto Tetris, surge la pregunta recurrente sobre si las diferentes piezas (I, O, T, L) deberían implementarse como clases que hereden de una clase base `Piece`.

Esta decisión aparentemente técnica esconde un problema más profundo: la tendencia a la sobreingeniería. Se observa con frecuencia cómo desarrolladores crean jerarquías de clases complejas cuando la funcionalidad requerida no lo justifica, añadiendo capas de abstracción innecesarias que complican el mantenimiento y la comprensión del código.

El dilema específico radica en que existe una presión constante por aplicar principios de orientación a objetos como la herencia, especialmente cuando se tienen entidades que parecen relacionadas conceptualmente. Las piezas del Tetris comparten características y comportamientos, lo que genera la intuición de que "deberían" formar una jerarquía de clases.

## ¿Qué?

La disyuntiva se centra en elegir entre dos enfoques arquitectónicos fundamentalmente diferentes: ¿aplicar herencia o mantener composición?

|Opción A: Aplicar herencia|Opción B: Mantener composición|
|-|-|
|Se crearían múltiples clases (`IPiece`, `OPiece`, `TPiece`, `LPiece`) que hereden de una clase base `Piece`. Este enfoque seguiría el principio tradicional de "es-un" donde cada pieza específica "es una" pieza. La jerarquía permitiría encapsular las diferencias en clases separadas y proporcionaría una estructura que muchos considerarían "más orientada a objetos".|Se conservaría una única clase `Piece` que recibe su forma como parámetro, utilizando un patrón Factory para la creación de diferentes tipos. Este enfoque trata las piezas como datos con comportamiento común, donde la diferenciación se logra mediante composición de diferentes formas.|

La tensión surge porque ambos enfoques son técnicamente correctos:

- La herencia cumpliría con los principios teóricos de la orientación a objetos.
- La composición mantiene la simplicidad práctica. 

El debate interno se intensifica al considerar que "las buenas prácticas" sugieren usar herencia cuando existe una relación jerárquica clara, pero la realidad del problema específico no presenta comportamientos diferenciados que lo justifiquen.

Esta dualidad genera incertidumbre: ¿se está siendo demasiado simplista al evitar la herencia, o se está siendo pragmático al evitar la sobreingeniería?

## ¿Para qué?

Las consecuencias de cada decisión son significativamente diferentes y afectan múltiples aspectos del desarrollo y mantenimiento.

**Si se aplica herencia:** Se obtendría una estructura que aparenta mayor "profesionalismo" arquitectónico, con cinco archivos que siguen principios clásicos de orientación a objetos. Cada tipo de pieza tendría su propia identidad de clase, lo que podría facilitar extensiones futuras específicas por tipo. Sin embargo, esto resultaría en código redundante donde cada clase derivada solo difiere en su constructor, violando el principio de responsabilidad única al crear clases que no aportan comportamiento diferenciado.

La complejidad se incrementaría sin beneficio funcional inmediato. Cualquier cambio en la lógica común requeriría modificaciones en la clase base, pero también verificación en todas las clases derivadas. Las pruebas unitarias se multiplicarían, necesitando verificar que cada subclase mantiene el comportamiento correcto.

**Si se mantiene composición:** Se conservaría la simplicidad actual con solo dos archivos principales que proporcionan toda la funcionalidad requerida. La extensibilidad se mantiene alta, ya que agregar nuevos tipos de piezas requiere únicamente modificaciones en el factory. Los cambios en la lógica común se aplican automáticamente a todos los tipos, eliminando inconsistencias potenciales.

La mantenibilidad se maximiza al concentrar toda la lógica en una ubicación central. Las pruebas se simplifican al tener una superficie de código menor que verificar. Sin embargo, podría percibirse como una solución "menos elegante" desde una perspectiva puramente teórica de orientación a objetos.

El criterio de decisión fundamental se resume en una pregunta específica: ¿existen comportamientos únicos por cada tipo de pieza que justifiquen la complejidad adicional de la herencia?

## ¿Cómo?

La aplicación de este criterio al caso específico del Tetris revela que todas las piezas comparten exactamente los mismos comportamientos: movimiento lateral, caída, rotación e interacción con el tablero. La única diferencia radica en los datos iniciales (la forma), no en la funcionalidad.

La implementación mantenida utiliza una clase `Piece` única que encapsula todo el comportamiento:

```java
class Piece {
    private boolean[][] shape;
    private Position position;
    
    public Piece(boolean[][] shape) {
        this.shape = shape;
        this.position = new Position(0, 0);
    }
    
    public void moveDown() {
        position.setY(position.getY() + 1);
    }
    
    public void rotateClockwise() {
        // Lógica común aplicable a todas las piezas
    }
}
```

El patrón Factory resuelve la diferenciación sin complejidad adicional:

```java
public class PieceFactory {
    public static Piece createIPiece() {
        boolean[][] shape = {{ true, true, true, true }};
        return new Piece(shape);
    }
    
    public static Piece createTPiece() {
        boolean[][] shape = {
            { false, true, false },
            { true, true, true }
        };
        return new Piece(shape);
    }
}
```

La validación de esta decisión se realiza aplicando el test de funcionalidad específica: al examinar cada tipo de pieza, se confirma que ninguna requiere métodos únicos, estados especiales o interacciones diferenciadas con otros elementos del juego.

Para casos futuros donde sí se justifique comportamiento específico, la extensibilidad se mantiene abierta mediante composición opcional:

```java
interface PieceSpecial {
    void activateSpecial(Board board);
}

class Piece {
    private PieceSpecial special; // null para piezas normales
    
    public void activateSpecial(Board board) {
        if (special != null) {
            special.activateSpecial(board);
        }
    }
}
```

Esta implementación demuestra que la decisión correcta no siempre es la más "teóricamente elegante", sino la que mejor se adapta a los requisitos reales del problema específico. La herencia debe justificarse con comportamientos únicos, no con datos únicos.