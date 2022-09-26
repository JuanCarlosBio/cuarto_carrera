# **Prácticas de Genética Evolutiva. 2021-2022**


## ***Actividades a realizar***

1) **Completar una [tabla](https://github.com/Juankkar/cuarto_carrera/blob/main/GEVO/practicas_gevo.csv) con los genotipos (frecuencias y absolutos).**

A partir de la tabla anterior había que hacer los puntos 2 y 3.

2) **Sacar la frecuencia absoluta de los alelos.**
3) **Rellenar una tabla en la que había que sacar:**
    - Frecuencia relativa alélica.
    - Heterocigosidad esperada y observada.
    - Realizar Xi^2 de contingencia.
    - Realizar Xi^2 para verificar equilibrio de Hardy-Weinberg.

El punto 4 es aparte. 

4) **Desequilibrio gamético mediante el método de Langley**.

---

## ***Plantilla de prácticas resuelta en R, hay dos Scripts:***

* Uno que se llama [funciones.R](https://github.com/Juankkar/cuarto_carrera/blob/main/GEVO/funciones.R), que tiene una serie de funciones que he programado para realizar los análisis. No tienes por qué mirarlo, ya que con una función llamada ```source()``` se descargan en el siguiente script de manera automática. Si es verdad que las puedes usar para cualquier ejecicio, cambiando los parámetros que necesites.

* El otro tiene la resolución de la plantilla de prácticas, llamado [practicas_genetica.R](https://github.com/Juankkar/cuarto_carrera/blob/main/GEVO/practicas_genetica.R). **Está hecho para que, en caso de que no hayan cambiado el formato de la pantilla, simplemente cambiando los valores de mi curso por los tuyos, debería correr perfectamente. De nada.**

* Ahora bien si han añadido alguna columna/fila o en caso de que no te salgan porque no entiendas nada de R pues sorry... 

###### jaja que prim@ no? xd

* Servir te pueden seguir serviendo, pero no te va a hacer la cama.

* Tengo algo mal por ahí, me pusieron un **9.6** (muy disrespectful eso xd), y no te dicen en que fallas. Creo que es lo último, lo de decir que gametos están en exceso o no.

* El script [**practicas_genetica.R**](https://github.com/Juankkar/cuarto_carrera/blob/main/GEVO/practicas_genetica.R) tiene una serie de comentarios, los cuales te recomiendo que leas. Te cuentan lo que estoy haciendo y los resultados que salen. En caso de escribir ***OJO!!!***, es que es probable que tengas que cambiar una cosa en concreto:

  * ***Línea 25***: vas a tener que cambiar los datos de la tabla original [**practicas_gevo.csv**](https://github.com/Juankkar/cuarto_carrera/blob/main/GEVO/practicas_gevo.csv), por los tuyos.

  * ***Línea 215... y el resto de líneas con la función de la chi cuadrado de HW***, ```HWChisq()```, la opción ```cc=...``` tienes que poner **0.5**, en caso de que sea necesario la ***corrección de Yates***, o **0** en caso de que no lo sea. ***Esto es de lo que más atento vas a tener que estar!***.

  * ***Línea 284***: en el valor de ***N*** (tabla de desequilibrio gamético) tendrías que poner los datos que te den en tu año.

  * ***Línea 315***: la tabla para hacer el método de Langley, básicamente lo mismo que el punto anterior.

---





