# Proyecto-1-MiniLisp

Integrantes del proyecto: 
                Isaac Rivera Jimenez\\
                No.Cuenta: 321225087\\
                González Gutiérrez Antonio Tonatiuh\\
                No.Cuenta: 321195476 \\
                Cervantes Farias Santiago\\                
                No. Cuenta: 321155797\\
• Instrucciones de Uso
Instrucciones de instalación
    Instalar Haskell: Asegúrese de tener instalada la plataforma Haskell. Puede descargarla desde \url{https://www.haskell.org/platform/}.
    Instalar Alex y Happy: Estas herramientas son necesarias para el analizador léxico y el analizador sintáctico. Instálelas mediante:
    cabal update
    cabal install alex happy
    Clone el repositorio:
    git clone <repository-url>
    cd Proyecto-1-MiniLisp

Instrucciones de ejecución
    Compila el proyecto:
    ghc --make Main.hs -o MiniLisp
    Ejecuta el intérprete:
    
En este proyecto se presentan un total de 8 archivos que permiten la ejecucion completa del lenguaje, la composicion del programa esta dada de la siguiente forma: 
            1.ASA.hs 
            2.ASAValues.hs
            3.Desugar.hs
            4.SASA.hs
            5.Interprete.hs
            6.Main.hs
            7.Lexer.x (Lexer.hs)
            8.Parser.y (Parser.hs)
1. Denota las reglas de Sintaxis Abstracta propuestas en la formalizacion del lenguaje
2. Define los valores finales de las reglas de sintaxis abstracta, proporcionando los valores de ejecución resultantes dentro del lenguaje.
3. Implementa la desazucarización del azúcar sintáctico, simplificando las reglas de sintaxis abstracta para facilitar su comprensión y uso.
4. Contiene las reglas de sintaxis abstracta azucarada, utilizadas por Desugar.hs e Interpreter.hs. Estas reglas permiten la transformación de la sintaxis azucarada a la sintaxis abstracta central para un funcionamiento más sencillo.
5. Se implementan las reglas de semantica operacional propuestas para la interaccion con el lenguaje extendido
6. Se presentan casos de prueba, asi como tambien parte de las facilidades del menu interactivo con el usuario
7.Implementado con la herramienta Alex, este archivo define la gramática del lenguaje de programación.
8.Implementado con la herramienta Happy, este archivo se encarga del análisis sintáctico y las expresiones regulares dentro de la formalización del lenguaje.
• Casos de prueba que demuestren la correcta ejecucion de las funciones y construcciones solicitadas.

• Guia del menu interactivo.
Se hace uso de un menú interactivo para facilitar la ejecución y prueba de programas escritos en el lenguaje de programación desarrollado. El menú permite al usuario:

Ingresar y evaluar expresiones individuales.

Cargar programas completos desde archivos.

Ver resultados de la evaluación paso a paso o de manera completa.

Probar funciones predefinidas y operaciones aritméticas, lógicas y sobre estructuras de datos como listas y pares.

Salir del intérprete de manera controlada.

El diseño interactivo busca que el usuario no necesite modificar el código fuente para ejecutar ejemplos o probar nuevas expresiones, haciendo el lenguaje más accesible y amigable.
  
• Documentacion interna del codigo 
Comentarios:

Cada módulo, función y tipo de dato incluye comentarios descriptivos que explican su propósito, parámetros y comportamiento esperado.
Se documentan casos especiales y restricciones importantes, como la necesidad de valores atómicos en ciertas operaciones o limitaciones de recursión.
Se incluyen ejemplos de uso para funciones complejas o combinadores, facilitando la comprensión del flujo de evaluación.

Organización de módulos:

ASAValues.hs: Define los tipos de datos abstractos del lenguaje (números, booleanos, listas, pares, expresiones, closures).
Desugar.hs: Contiene funciones para transformar sintaxis de alto nivel en construcciones más simples que el intérprete pueda procesar.
Interprete.hs: Implementa la lógica de evaluación paso a paso (stp), evaluación estricta (stpStrict) y la función de interpretación completa (interp).
SASA.hs / Main.hs: Contienen la interfaz del menú interactivo y la integración de los módulos del lenguaje para ejecutar programas completos.

Los módulos están diseñados para mantener separación de responsabilidades: definición de datos, transformación de sintaxis, evaluación y presentación al usuario.

Funciones auxiliares documentadas:

lookupEnv: búsqueda de variables en el entorno.
isValueV: determina si una expresión es un valor final.
stpArgs: evalúa argumentos de funciones aplicadas de manera estricta.
stpStrict: fuerza evaluación de expresiones parciales siguiendo los puntos estrictos.

Notas sobre estilo y mantenimiento:

Se sigue una convención consistente para nombres de funciones y tipos (CamelCase para tipos, lowerCamelCase para funciones).
Se prioriza claridad y legibilidad sobre optimización prematura, facilitando futuras extensiones del lenguaje.

Se documentan errores esperados y excepciones, como variables no encontradas, división por cero y patrones incompletos.
