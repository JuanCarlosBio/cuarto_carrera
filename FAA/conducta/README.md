# Prácticas de la conducta de ratones.

## Estudiamos la conducta de ratones según su edad (jóvenes y viejos) en una serie de pruebas.

* Open Field: los ratones andaban por un contenedor subdividido imaginariamente por distintas zonas que servían como variables de estudio:
   - Ambulaciones externas: por el exterior del contenedor.
   - Ambulaciones internas: una región más hacia adentro.
   - Ambulaciones al centro del contenedor.
   
* Hole Maze: una caja abierta con unos agujeros donde se colocaba al ratón, se estudiaba:
  - Ambulación general por la caja.
  - Detenerse a meter la cabeza en un agujero.
  - Tiempo de olisqueo de un agujero.
  
* Laberinto cruz elevado (P.maze). Los ratones estaban en un laberinto sencillo en forma de cruz. Había dos tipos de brazos a cada lado, los abiertos y los cerrados. Se estudió:
  - Tiempo que tardaban en cada brazo y en la zona central.
  - Número de ambulaciones en cada brazo y centro.

* Además había otros comportamientos específicos que se notificaban en todos las pruebas. En algunas nosotros no tomamos alguno de estos parámetros por equivocación.
  - Nº Grooming (acicalamiento del ratón). 
  - Nº Heces.
  - Nº de Ups (saltos), las veces en las que se ponían de dos patas tanteando el terreno.

## Resultados 

Para corroborar la distribución de los datos y la igualdad de varianzas, Se testearon mediante las pruebas de Shapiro-Wilks y Levene respectivamente.

### **Open Field:**

---

<p align="center">
 
  <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/conducta/graficas/om.png" alt="900" width="900" />

</p>

**Figura 1**. Muestra los resultados de Open Field.

Ninguna de las comparaciones en esta prueba reflejaron diferencias significativas entre los ratones viejos y jóvenes. La figura presenta una errata, el test utilizado para **C** debido a la falta de normalidad de los datos fue Wilcoxon. Por eso se muestra un boxplot, para ver la mediana y la distribución de los valores en ese caso en concreto. Por si no lo he cambiado.  

---

### **Hole Maze:**


---

<p align="center">
 
  <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/conducta/graficas/hm.png" alt="800" width="800" />

</p>

**Figura 2**. Muestra los resultados de Hole Maze. 

En este caso, todas las muestras fueron normales y presentaban homocedasticidad. La variable de tiempo fue separada del resto aun así por presentar un eje y muy largo. Solo se observó diferencias significativas en las veces que los ratones meten la cabeza en los agujeros, presentando los ratones viejos un promedio de veces mayor que los jóvenes.

Estos resultados sorprenden, ya que se esperaría que fueran los ratones viejos los que tuvieran mayor sentido aventurero por su juventud.

---

### **Laberinto en cruz suspendido**.

---

<p align="center">
 
  <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/conducta/graficas/Rplot05.png" alt="900" width="900" />

</p>

**Figura 3**. Resultados del P.maze.

En este caso, se tuvieron que filtrar a una serie de outliers. Fue la primera prueba y seguramente algunos de los ratones se pusieran nerviosos, o no estaban preparados para ser sometidos a este estrés. En ese sentido los primeros ratones pasaron un tiempo exagerado en ciertas zonas. Con lo que se procedió a prescindir de estos outliers.

En concreto fueron:

```
p.maze
# A tibble: 3 x 3
  v_j     observacion      Tiempo (seg) 
  <chr>   <chr>               <dbl>
1 Viejos  Tiempo centro         838
2 Viejos  Tiempo cerrado       1753
3 Jovenes Tiempo centro        1515
```

Solo hubo diferencias significativas en las estancias del centro. No encontrándose nada en los otros brazos, aún así se atisba que alguno de los jóvenes sí que eran más aventureros en los brazos largos (más estrés para ellos) boxplot, pero eso nada significativo. El centro sería la segunda zona de más "riesgo" y menos protección para estos animales, con lo que puede tener sentido estos resultados en términos de brabería de los más jóvenes.

---

Sin embargo, se esperaría haber encontrado mayor número de diferencias entre estas dos edades. Habría que ver la data de los otros compañeros.

## **Análisis multivariante de las tres pruebas**

Procedí por mi cuenta a realizar un PCA para cada una de las pruebas, con la intención de ver si encontraba algún patrón.

---

<p align="center">
 
  <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/conducta/graficas/Rplot07.png" alt="900" width="900" />

</p>

**Figura 4**. PCA para cada prueba. 

Se puede ver que a pesar de no haber las diferencias significativas esperadas entre los grupos de ratones jóvenes y viejos (No he realizado la inferencia en cuanto a las componentes, al menos por el momento), hay un patrón en todos gráficos.

Parece ser que los ratones de mayor edad sufren menor dispersión en los trabajos que realizaban en las tres pruebas, tanto en las PC1 como en las PC2 de manera general. Con lo que a pesar de que no encontramos diferencias significativas, los viejos son más "conservadores" en el trabajo que realizan, seguramente debido a la experiencia adquirida con el tiempo, comportándose todos de manera parecida. Mientras que los ratones jóvenes al ser inexpertos, pues cada uno podría estar probando una cosa distinta y explorando más el mundo a su alrededor.

Hay un pero en el Gráfico de P.maze. La varianza no llega al 60% con lo que no es del todo fiable. La elipse de los ratones viejos parece ser al igual que en las dos pruebas anteriores igual de reducida, pero es más circular. Al realizar un gráfico 3D (usándose de esta manera la PC3, pasando a tener un ~73%) se vio que dos de los ratones viejos están separados hacia atrás, pero el resto están bastante juntos. Parecería seguir cumpliéndose la lógica del párrafo anterior.

<p align="center">
 
  <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/conducta/graficas/Rplot06.png" alt="900" width="900" />

</p>

**Figura 5.** Gráfico 3D interactivo de Plotly para P.maze. En ese sentido te recomiendo que veas el script para cerciorarte tu mismo debido a que al peso no puedo subirlo aquí =).

---
