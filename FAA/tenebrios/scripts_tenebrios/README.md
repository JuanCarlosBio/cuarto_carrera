# Prácticas de los tenebrios.

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

# Resumen de los resultados

## Resultados de nuestro experimento en concreto:

En concreto a mi grupo y a mí nos tocó el experimento de hacer crecer a los tenebrios en oscuridad (eramos el 4º grupo de este en concreto).

**Antes de continuar, en este caso he contabilizado la primera semana como semana 0 en este caso. Creo que para el estudio de todos los grupos la primera la contabilicé como semana 1. Un fallo de cálculo pero se va aquedar como está xd.**

* Comenzando con la variación de la masa corporal, como era esperable, la masa de los tenebrios crecía de manera casi exponencial, hasta que en las últimas semanas se atisba la formación de una curva que muestra el tope.

* Para la tasa de crecimiento, recordemos que consiste en el resultado de restar la masa corporal de, en el caso de la primera muestra, de la semana 1 y la semana 0 (masa_corporal_semana1 - masa_corporal_semana0). En ese sentido, la tasa de crecimiento está en auge, preo no en todas las semanas, hay algunas como la semana 1 que sufre un bajón (no crecieron esa semana, igual porque a lo mejor se nos murió uno de las larvas y tuvimos que reemplazarla por otra más joven), sin embargo vuelve a subir en la sigueinte semana, donde se queda estable hasta la semana 4 y luego en las semanas 5 y 6 llega a su pico. A partir de ahí sufre una caída en picado, coincidiendo con la entrada en la curva de la masa corporal.

---

<p align= "center">
  <img src= "https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/tenebrios/scripts_tenebrios/graficas/Rplot05.png">
</p>

---








