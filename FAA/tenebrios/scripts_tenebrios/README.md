# Prácticas de los tenebrios. (Sacamos un 9.5) 

## Información sobre las práctias.

* Durante el primer cuatrimestre tuvimos que cuidar a unas larvas de esta especie de escarabajos.
* Cada semana había que visitar los laboratorios del Área de Fisiología Animal, donde tomábamos las medidas del aumento de las masa corporal.

### Objetivos de las prácticas.

* A partir de la toma de la masa corporal de unas 12 larvas originales.
* En una práctica de informática te enseñaran como tomar los datos en Excel. El objetivo principal de la práctica es aprender además de como tomar los datos de la masa corporal, sacar a partir de ella la tasa de crecimiento, que se hace básicamente, por ejemplo, restanto la masa corporal de la 1º semana con la siguiente, es decir de la 2º (masa corporal 1º semana - masa corporal 2º semana), así hastas la última semana. Por eso mismo, la gracia de la práctica es aprender a como automatizar el proceso en Excel, o en mi caso en R (este no te lo enseñan obvio xd). Luego la tasa de crecimiento diaria es obviamente la semanal/7 días.
* El experimento dura hasta que el 50% de tus larvas se convierten en pupa. 
* Una vez que todos los grupos finalicen, además de analizar tus datos, hay que hacerlo del resto.
La verdad es que una práctica muy interactiva, a mí me gustó mucho

### Vamos en resumen hay que:
* Realizar una memoria de tus datos y la de tus compañeros estudiando las siguientes variables...
* Masa Corporal --> aumento del peso semanal.
* Tasa de cecimiento semanal --> lo que han crecido en la semana
* Tasa de crecimiento diaria --> lo que han crecido diariamente

## En ese sentido, entiendo que puedo tener un poco desordeando esto pero:
* El directorio tiene dos scripts: tenebrios.R con los resultados del nuestro grupo, es decir el Grupo 4 de Oscuridad. Por otro lado el script de tene_todos_limpio.R es el que utilicé para estudiar los resultados de todos.
* Por otro lado hay 3 bases de datos:
* Nuestros resultados: están por un lado tenebrios.csv --> tal cuál como te piden hacer la base de datos (la media de la masa corporal cada semana), las tasas de crecimiento semanal y diaria. Mientras, df_tenebrios.csv, los datos de cada uno de los tenebrios pesados cada semana.
* Resultados de todos: tene_todos_crudos.R --> la base de datos de todos los alumnos de FAA. He de decir que la forma que toman los datos, queda muy bonito, pero no es muy R, SPSS, Python... friendly, con lo cual tuve que hacer un preprocesamiento guapo. 

# ***Resumen de los resultados*** (Son además más de lo que llegamos a presentar)

## **Resultados de nuestro experimento en concreto:**

En concreto a mi grupo y a mí nos tocó el experimento de hacer crecer a los tenebrios en oscuridad (eramos el 4º grupo de este en concreto).

**Antes de continuar, en este caso he contabilizado la primera semana como semana 0 en este caso. Creo que para el estudio de todos los grupos la primera la contabilicé como semana 1. Un fallo de cálculo pero se va a quedar como está xd.**

* Comenzando con la variación de la masa corporal, como era esperable, la masa de los tenebrios crecía de manera casi exponencial, hasta que en las últimas semanas se atisba la formación de una curva que muestra el tope.

* Para la tasa de crecimiento, recordemos que consiste en el resultado de restar la masa corporal de, en el caso de la primera muestra, de la semana 1 y la semana 0 (masa_corporal_semana1 - masa_corporal_semana0). En ese sentido, la tasa de crecimiento está en auge, preo no en todas las semanas, hay algunas como la semana 1 que sufre un bajón (no crecieron esa semana, igual porque a lo mejor se nos murió uno de las larvas y tuvimos que reemplazarla por otra más joven), sin embargo vuelve a subir en la sigueinte semana, donde se queda estable hasta la semana 4 y luego en las semanas 5 y 6 llega a su pico. A partir de ahí sufre una caída en picado, coincidiendo con la entrada en la curva de la masa corporal.

---

<p align= "center">
  <img src= "https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/tenebrios/scripts_tenebrios/graficas/Rplot05.png">
</p>

---

Por otro lado además, realizamos un test de correlación lineal de pearson, que nos permitieran ver si las variables de la ***masa corporal*** y la ***tasa de crecimiento*** estaban correlacionadas linealmente con las semanas, y de esa manera ser capaces de hacer un modelo de regresión lineal simple, con la intención de sacar futuros valores en otros experimentos.

* **Las variables de la masa corporal y la semana muy están correlacionadas etre ellas**

```
	Pearson's product-moment correlation

data:  df_o4_completo$semana and df_o4_completo$media_mc
t = 24.251, df = 9, p-value = 1.649e-09
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9700914 0.9981027
sample estimates:
      cor 
0.9924351 
```

* **Por otro lado, las variables tasa crecimiento - semanas; masa corporal - tasa de crecimiento, respectivamente estaban prácticamente correlacionadas de forma nula**

```
	Pearson's product-moment correlation

data:  df_o4_completo$tasa.crec.s and df_o4_completo$semana
t = 0.061966, df = 8, p-value = 0.9521
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.6162214  0.6426665
sample estimates:
      cor 
0.0219031 

	Pearson's product-moment correlation

data:  df_o4_completo$media_mc and df_o4_completo$tasa.crec.s
t = -0.12568, df = 8, p-value = 0.9031
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.6556916  0.6020620
sample estimates:
        cor 
-0.04439224 
```

* **Con lo cuál sólo hicimos el modelo para la primera opción.** 
	* Sacamos los coeficientes de la pendiente e intecepto.
	* El modelo presentó un p < 0.01, con lo que se rechazá la hipótesis nula que el modelo no es fiable.

```
Call:
lm(formula = media_mc ~ semana, data = df_o4_completo)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.004655 -0.004347 -0.001590  0.004509  0.006998 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.0044540  0.0032423   1.374    0.203    
semana      0.0115933  0.0004781  24.251 1.65e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.005014 on 9 degrees of freedom
Multiple R-squared:  0.9849,	Adjusted R-squared:  0.9833 
F-statistic: 588.1 on 1 and 9 DF,  p-value: 1.649e-09
```

Gráfica en ese entonces, tanto con la media de los valores de la masa corporal (como en el primer gráfico), pero añadiendo las barras de error con la desviación típica y la función de la recta del modelo.

---

<p align= "center">
<img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/tenebrios/scripts_tenebrios/graficas/Rplot07.png">
</p>

---

## **Resultados de todos los grupos:**

### Masa corporal de todos los grupos
Lo que pudimos ver es que, al hacer la media de cada uno de los grupos de cada uno de los tratamiento para cada semana fue:

* Parece ser que todos los grupos crecen más o menos de forma exponencial, hasta formar una curva final en la que las larvas para de crecer en masa.
* Parece ser que en cuanto a crecimiento: 
	* El grupo control (luz y temperatura normal) son los que menos crecen.
	* Los de oscuridad crecen de forama intermidia hasta adelantar en las últimas semanas a temperatura alta, que hasta la semana 9 son os que más masa tienen. Esto puede deberse a que en este último tratamiento las larvas podrán pasar más rápido a estado de pupa, perdiendose datos de este. 
* Pero ... ¿Son las diferencias realmente significativas al comparar los tratamientos en cada semana? Relizaremos inferencia estadistica para ello (ojo!! de las 9 primeras semanas, creo xd, ya que a partir de la 10, algunos de los grupos habían terminado y daba error en los modelos por falta de datos, pero bueno, más o menos en esta semana se puede considerar pico por estar en la curva, con lo que me sirve).
	* **Lo que vimos es que ninguno de los grupo presentó diferencias significativas!!**
	* De esta manera, si te interesa de que las larvas tengan más masa, da igual el tratamiento a usar.

---

<p align= "center">
<img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/tenebrios/scripts_tenebrios/graficas/masa_corporal_todos.png">
</p>

---

### Tasa de creciemiento

En este caso:
* Parece ser que todos aumentan en masa o se estacionan de manera estable desde la semana 1 a la 7 (en osuridad curiosamente todos para todos los grupo también sufren como un bajón en la semana 2, es algo en lo que me he fijado, puede ser un peródo de adaptación por el estrés que están en este experimento a lo mejor).
* A partir de la semana 7, va decayendo la tasa. La semana 11 de oscuridad es un outlier seguro (sólo quedaba un grupo para cada experimento).
* Hicimos de nuevo inferencia hasta la semana 8, de nuevo por la entrada en pupa en las últimas semanas.
	* **De nuevo ninguno de los grupo presentó diferencias significativas entre tratamientos para cada una de las semanas de comparación** (si no se me ha ido la pinza)  
	* Si te interesa la tasa de crecimiento, da igual en ese entonces te sirve cualquier grupo.

---

<p align= "center">
<img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/tenebrios/scripts_tenebrios/graficas/Rplot04.png">
</p>

---

### **¿Entran en estado de pupa antes dependiendo del experimento en cuestión?**

En ese entoces, la pérdida de valores de las últimas semanas debido a la entrada de escarabajos en pupa nos hizo plantearnos la pregunta de arriba. Vale, no hay diferncias en el aumento de la masa corporal, tasa de crecimiento semanal... pero parece ser que los del expermento de temperatura alta entran antes en pupa, lo cual puede ser interesante para alguna empresa sacarle rédito económico.

* Realizamos entonces inferencia estadística de las semanas de estudio
	* **No hubo diferencias, se intentó wua wua, pero que guapo quedó en el trabajo xd**.


---

<p align= "center">
<img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/tenebrios/scripts_tenebrios/graficas/Rplot03.png">
</p>

---



