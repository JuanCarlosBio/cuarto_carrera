# **Resultados de las artemias 2022 de todos los grupos.**

Si eso en algún momento consultaré la bibliografía para encontrar información interesante y sacar alguna conclusión de estos resultados y lo complemento con el trabajo que hice yo en su momento. Pero la verdad es que en el momento que estoy haciendo esto, no tengo todo el tiempo libre que me gustaría.

## Ácidos grasos de estudio

* *Ác. palmítico:* **C 16:0**
* *Ác. palmitoleico* : **C 16:1n-7**
* *Ác. esteráico:* **C 18:0**
* *Ác. oleico:* **C 18:1n-9**
* *Ác. linoleico:* **C 18:2n-6**
* *Ác. gamma linoléico:* **18:3n-6**
* *Ác. linolénico:* **C 18:3n-3**
* *Ác. esterearidónico:* **C 18:4n-3**
* *Ác. araquidónico:* **C 20:4n-6 (ARA)**
* *Ác. timmodónico:* **C 20:5n-3 (EPA)**
* *Ác. docosahexanoico:* **C 22:6n-3 (DAG)**
## * Gráfico con los resultados obtenidos de todos los ácidos grasos.
---

<p align="center">
  <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/artemias_2022/artemias_2022.png">
</p>

**Figura:** se me ocurrió, como no todos los valores eran normales, y en ese caso estudiaremos la mediana en ese caso cuando sea necesario, he hecho boxplots con una modificación. Se mostrarían la distribución de los datos (mínimos, máximos, cuartiles) y la mediana como línea negra. Pero además he añadido el estadístico de la media a partir de una nueva línea de color rojo, para poder fijarnos. Con lo cuál nos podemos fijar en un estadístico o en otro. Como no se verían bien los resultados, (ya que hay algunos AG, con porcenjates muy superiores) además he subdividido en casillas cada ácido graso. Esto anterior tiene un problema, y es que el eje de las y se ajusta automáticamente para cada una, por defecto, cada casilla ocupa la mínima distancia entre ácidos grasos, con lo cuál puede engañar un poco a la vista si quieres buscar diferencias.

Yo creo, que lo mejor para solucionar los problemas de esta figura lo ideal sería buscar una forma de separar los gráficos según un sentido lógico fisiológico. En plan ácido grasos eseciales, polares, cadena saturada/insaturada. Pero me voy a quedar con la figura que he hecho, no creo que esté mal del todo, y además no es que sea un experto en la materia de AGs.

---

* **Nota importante antes de continuar:** 
  * No encontré en tres de los tratamiento rastros del  ác. gamma linoleico, con lo cual, alginos de los test no corren cuando son sólo valores 0. Pero es obvio que existen por un lado diferencias significativas, al menos en uno de los grupos. En este caso, sólo el tratamiento de aceite de echium bacalao presenta este ácido graso. 
  * Alomejor me he dejado de identificar algún AG que pedía la profesora, yo he identificado los que hicimos en la práctica de informática y listo. 

### **En cuanto a la inferencia estadística, estos fueron los resultados:**

* **Los AG que cumplieron los requisitos para ANOVA de una vía fueron:**
  - C 16:0 
  - C 18:1n-9
  - C 18:2n-6
  - C 18:4n-3
  - C 20:5n-3
  - C 22:6n-3 
  
En todos los casos, el modelo de ANOVA dió ***p*** **< 0.05**

```
A tibble: 6 x 2
  acido_graso        p
  <chr>          <dbl>
1 16:0        1.39e- 7
2 18:1n-9     5.95e- 7
3 18:2n-6     1.32e-10
4 18:4n-3     4.18e- 8
5 20:5n-3     1.11e- 6
6 22:6n-3     5.39e- 7
```

Tukey, hay muchas comparaciones obviamente, si estas interesad@, puedes usar ```filter()``` en el script. En caso de querer ver todos los resultados puedes usar ``` ...%>% view()```, o ```... %>% print(n=Inf)```:

```
# A tibble: 33 x 4
   acido_graso comparacion1           comparacion2           significacion
   <chr>       <chr>                  <chr>                  <chr>        
 1 16:0        Echium/Bacalao         Enriquecedor comercial ****         
 2 16:0        Echium/Bacalao         Lectina marina         ****         
 3 16:0        Echium/Bacalao         Levadura               *            
 4 16:0        Enriquecedor comercial Lectina marina         ns           
 5 16:0        Enriquecedor comercial Levadura               ***          
 6 16:0        Lectina marina         Levadura               ****         
 7 18:1n-9     Echium/Bacalao         Enriquecedor comercial ns           
 8 18:1n-9     Echium/Bacalao         Lectina marina         ***          
 9 18:1n-9     Echium/Bacalao         Levadura               ****         
10 18:1n-9     Enriquecedor comercial Lectina marina         ns     
```
* **Los ácidos grasos normales pero no homocedásticos para hacer ANOVA de Welch por otro lado fueron:**
  - C 16:1n-7
  - C 18:0
  
```
A tibble: 2 x 2
  acido_graso         p
  <chr>           <dbl>
1 16:1n-7     0.0000197
2 18:0        0.002  
```

Games-howells:

```
# A tibble: 12 x 4
   acido_graso comparacion1           comparacion2           significacion
   <chr>       <chr>                  <chr>                  <chr>        
 1 16:1n-7     Echium/Bacalao         Enriquecedor comercial **           
 2 16:1n-7     Echium/Bacalao         Lectina marina         ***          
 3 16:1n-7     Echium/Bacalao         Levadura               **           
 4 16:1n-7     Enriquecedor comercial Lectina marina         ns           
 5 16:1n-7     Enriquecedor comercial Levadura               ***          
 6 16:1n-7     Lectina marina         Levadura               **           
 7 18:0        Echium/Bacalao         Enriquecedor comercial ns           
 8 18:0        Echium/Bacalao         Lectina marina         ns           
 9 18:0        Echium/Bacalao         Levadura               **           
10 18:0        Enriquecedor comercial Lectina marina         ns           
11 18:0        Enriquecedor comercial Levadura               *            
12 18:0        Lectina marina         Levadura               ns   
```

* **Por último, los que no presentaban normalidad en la distribución de los datos, y por ello se usó Kruskal-Wallis:**
  - C 18:3n-3
  - C 20:4n-6

```
# A tibble: 2 x 2
  acido_graso       p
  <chr>         <dbl>
1 18:3n-3     0.00428
2 20:4n-6     0.00481
```

Test de Dunnet (corrección de Bonferroni):

```
# A tibble: 12 x 4
   acido_graso comparacion1           comparacion2           significacion
   <chr>       <chr>                  <chr>                  <chr>        
 1 18:3n-3     Echium/Bacalao         Enriquecedor comercial ns           
 2 18:3n-3     Echium/Bacalao         Lectina marina         **           
 3 18:3n-3     Echium/Bacalao         Levadura               ns           
 4 18:3n-3     Enriquecedor comercial Lectina marina         ns           
 5 18:3n-3     Enriquecedor comercial Levadura               ns           
 6 18:3n-3     Lectina marina         Levadura               ns           
 7 20:4n-6     Echium/Bacalao         Enriquecedor comercial **           
 8 20:4n-6     Echium/Bacalao         Lectina marina         ns           
 9 20:4n-6     Echium/Bacalao         Levadura               ns           
10 20:4n-6     Enriquecedor comercial Lectina marina         ns           
11 20:4n-6     Enriquecedor comercial Levadura               ns           
12 20:4n-6     Lectina marina         Levadura               ns    
```

## **Análisis de componentes principales**

### **Resultados**

---

<p align="center">
<img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/artemias_2022/artemias_pca2022.png">
  </p>

Es curioso cuanto menos que los resultados de las prácticas de informática sean muy parecidos, si no idénticos, a los resultados obtenidos para enero de 2022.

Esto puede deberse a que, a dos motivos.

* *Hay evidencia de que este experimento es reproducible o parcialmente al menos. Obetiéndose los mismos resultados a partir de los mismos pasos.*
* *Pero sin embargo, me suena que como la práctica era muy próxima a las evaluaciones de enero, con lo que no había tiempo físico para los profesores en usar nuestras muestras. Con lo cual estos resultados serían de alumnos del curso pasado, que se podrían haber sido usados tanto para las prácticas de informática como para esta.*
* *Aún así, el segundo caso sigue defendiendo que el experimento es reproducible, en caso de que se hicieran dos veces para cada práctica. Pero eso yo ya no lo sé. Simplemente es una observación.*
---  
### En cuanto a los resultados de inferencia para las componentes:

PC1: Se usó Kruskal-Wallis y debido a diferencias significativas, se usó como siempre el test de Dunnet (corrección de Bonferroni).

```
# A tibble: 6 x 4
  componentes comparacion1              comparacion2              significacion
  <chr>       <chr>                     <chr>                     <chr>        
1 PC1         "Levadura"                "Lectina\nmarina"         ns           
2 PC1         "Levadura"                "Aceite\nEchium\nBacalao" ns           
3 PC1         "Levadura"                "Enriquecedor\ncomercial" ns           
4 PC1         "Lectina\nmarina"         "Aceite\nEchium\nBacalao" **           
5 PC1         "Lectina\nmarina"         "Enriquecedor\ncomercial" ns           
6 PC1         "Aceite\nEchium\nBacalao" "Enriquecedor\ncomercial" ns       
```

PC2: Se usó Games-Howels

```
# A tibble: 6 x 4
  componentes comparacion1              comparacion2              significacion
  <chr>       <chr>                     <chr>                     <chr>        
1 PC1         "Levadura"                "Lectina\nmarina"         ***          
2 PC1         "Levadura"                "Aceite\nEchium\nBacalao" ***          
3 PC1         "Levadura"                "Enriquecedor\ncomercial" ***          
4 PC1         "Lectina\nmarina"         "Aceite\nEchium\nBacalao" ****         
5 PC1         "Lectina\nmarina"         "Enriquecedor\ncomercial" ****         
6 PC1         "Aceite\nEchium\nBacalao" "Enriquecedor\ncomercial" ****    
```
