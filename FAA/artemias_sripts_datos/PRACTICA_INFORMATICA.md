# **Desarrollo de las prácticas de Informática Artemias Fisiología Animal Aplicada ULL**

---
## Intención: 

*  Buenas, mi intención con este Markdown es mostrar como replicaría la práctica de informática de artemias de la asignatura de ***Fisiología Animal Aplicada, Grado de Biología, de la Universidad de La Laguna*** con el lenguaje de programación R. Debido a que me pareció de las prácticas más importantes de cuarto por la falta de alguna asignatura de análisis de datos computacionales. Se trata más que nada comentar parte de lo que he hecho en los scripts, donde está todo resuelto y con algún comentario también. En teoría deben estar hechos de tal forma para que puedas copiar dichos scripts, pegarlos en tu ordenador y correrlo directamente (***recuerda descargar las librarías!!!*** --> ```install.packages("cualquier_libreria")```) 

* He de decir que no me considero una eminencia ni nada por el estilo con esta herramienta. Pero llevo entre 1 año y medio o 2 aprendiendo lo que puede ofrecer para nosotros los biólogos. Habiéndole pillado cierto tranquillo en el proceso.


* Para dejar las cosas claras, mi intención no es enseñar R de principio a fin... solamente comentar como he replicado la práctica de la forma en la que he aprendido ***yo*** a hacerlo con código propio. ***Hay miles de videotutoriales en youtube*** o en la red (***y gratis, no te vuelvas loc@***) que realizan ese cometido de manera brillante (tanto de R como otros como vendría a ser de Python). Algunas librerías que vendría bien haber visto antes serían, en especial: ***dplyr***, ```ggplot2``` o el conjunto de ```tidyverse```. También tener claro los modelos de inferencia y cuándo elegir uno u otro vendría bien. Utilizaremos una librería muy guapa que te permite hacer estos modelos en combinación con ```dplyr```, ```rstatix```. Y obviamente, debes tener R descargado (es ideal que además tengas Rstudio). Saber cosas básicas como descargar paquetes, asignar valores a objetos (que significa "<-")...

### **Nota: en caso de alguna duda puedes usar ```?``` al principio de la función que te presente dudas, te llevará a una página ejemplo:**

```
# Al final del todo de la página de ayuda hay ejemplos de su uso (por cierto, #  impide que R corra la línea 
# código en cuestión desde donde está colocada. Con lo que puedes comentar lo que quieras). 

install.packages("dplyr")
?library()
library(dplyr)
?mutate()

```

### **Pero bueno, independientemente de ello, si hay alguien viendo esto... espero que te sirva de ayuda, y si eres de 4º... Ánimo, que ya se acaba, fuerte hasta el final.**

---

## **Parte 1) Script de inferencia estadística: [inferencia_estadistica.R](https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/scripts_codigo/inferencia_estadistica.R)**

Comenzaremos con lo básico, las librerías principales a usar. Haré un comentario de cada una.

```
library(tidyverse) # Paquete con las librerías de dplyr (manipulación de las tablas de datos) y ggolot2 (realización de gráficas de calidad)
library(readxl)    # leer csv, xlsx...
library(rstatix)   # Modelos ANOVA, WELCH...
library(glue)      # permite pegar variables asignadas en texto mediante {}
```

Lo siguiente es poner a punto la base de datos. Vamos a correr los datos (```%>%``` es el pipe (tubería), conecta las funciones entre sí compatibles con ```dplyr```)

```

matrix_artemias <- read_csv("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/artemias_sripts_datos/matrix_artemias.csv")

matrix_artemias %>% print(5)

```

Veamos el output de las 5 primeras filas...

```
> matrix_artemias
# A tibble: 16 x 12
   Tratamiento `C 16:0` `C 16:1n-7` `C 18:0` `C 18:1n-9` `C 18:2n-6` `C 18:3n-6`
         <dbl>    <dbl>       <dbl>    <dbl>       <dbl>       <dbl>       <dbl>
 1           1    10.4         2.45     5.49        21.0        6.75        0.31
 2           1    10.5         2.46     5.52        20.6        6.55        0.37
 3           1    10.5         2.43     5.53        20.4        6.47        0.27
 4           1    10.8         2.47     5.66        21.3        6.38        0.26
 5           2    12.2         2.31     5.68        18.2        4.01        0.23

```

Tenemos 16 filas de valores de un total de 12 variables (nos fijamos en la segunda línea ```A tibble: 16 x 12```). Solo se ven 5 filas porque con la función ```print(n = 5)``` hago que me muestre ese número, mientras que se ven sólo 7 columnas porque el formato ***tibble*** (una forma de hacer las tablas en R) los resume de esa manera porque no se veían todas en la pantalla. 

---

* La primera variable es la categórica, **Tratamiento**. El grupo 1 = Levadura, grupo 2 = Lectina marina, grupo 3 = Aceite de *Echium*/Bacalao y grupo 4 = Enriquecedor comercial.
* Las otras variables son las numéricas de cada uno de los ácidos grasos de interés. Representan realmenta el % de área de dicho AG que es básicamente la cantidad de AG en la muestra. Nada de lo que preocuparse por ahora.

---

Esta conformación de los datos no es del todo ideal para R. Así que comenzaremos con lo básico de Ciencia de Datos, y es poner la base de datos a punto. 

* Lo primero es cambiar los números de ```Tratamiento``` por cada uno de sus valores categóricos. Para ello usamos la función ```mutate()``` (crea una nueva variable), y dentro de este crearemos una nueva variable usando ```case_when()```, que mediante un condición, usando el operador ```==```, pregunta: ¿es x_cosa igual a y_cosa?, y si es cierto con ```~``` le decimos que nos lo convierta en un valor que le asignemos.

* Usaremos ```select()```, selecciona variables que le pidamos, o en este caso si la variable tiene un "-" significa que nos deshacemos de esa variable (la variable original de ```Tratamiento``` con los números en vez de los nombres. Date cuenta de que la nueva creada la hemos llamado ```tratamiento``` en minúscula para que sea más sencillo trabajar con ella).

* Finalmente, una de las funciones que más me han cambiado la vida. A R (y otra clase de Lenguajes de scripting similares), le gustan más las bases de datos más estrechas (***formato "tidy"***), antes que anchas como esta. Es decir, se prefiere que presenten menos número de variables (*columnas*), pero más largas en cuanto *filas* (le permite hacer a la máquina los trabajos de manera más automática). Para ello usaremos ```pivot_longer()```. La idea es pasar de 12 variables a únicamente 3, ¿Cómo hace eso esta función?... suena como romper las leyes de la termodinámica..... es Magic.... Nah es broma. Ahora en serio, vamos a hacer que las columnas de tratamiento y los valores numéricos de las variables se coloquen encima una de las otras de forma ordenada (no te preocupes, R no se inventa nada), por otro lado, vamos a hacer que los nombres de cada variable numérica (los AG) se conviertan en la tercera columna. Llamaremos a la nueva variable donde están los valores numéricos ***"valores"*** y la que presenta los AG ***"acido_graso"*** (***como consejo***, cuando llames a una variable, hazlo en minúscula sin tildes, y la separación con "_" y evita la ñ espa**ñ**olete). Como **tratamiento** no nos interesa que se modifique, sino que se haga más *larga* únicamente, le añadimos un "-".


Sé que es una movida, pero **no asustarse** ahora veremos que pasa.

```
tidy_artemias <- matrix_artemias %>% 
  mutate(tratamiento = Tratamiento,
         tratamiento = case_when(tratamiento == 1 ~ "Levadura",
                                 tratamiento == 2 ~ "Lectina marina",
                                 tratamiento == 3 ~ "Echium/Bacalao",
                                 tratamiento == 4 ~ "Enriquecedor comercial")) %>% 
  select(-Tratamiento) %>% 
  pivot_longer(-tratamiento, names_to  = "acido_graso", values_to = "valores")

tidy_artemias
```

Veamos el output (Recuerden como estaban los datos al comenzar y comparenlos con como están ahora) ...

```
tratamiento     acido_graso  valores
   <chr>       <chr>         <dbl>
 1 Levadura    C 16:0        10.4
 2 Levadura    C 16:1n-7      2.45
 3 Levadura    C 18:0         5.49
 4 Levadura    C 18:1n-9     21.0
 5 Levadura    C 18:2n-6      6.75
 6 Levadura    C 18:3n-6      0.31
 7 Levadura    C 18:3n-3     23.5
 8 Levadura    C 18:4n-3      3.36
 9 Levadura    C 20:4n-6      1.35
10 Levadura    C 20:5n-3      2.07
# ... with 166 more rows
```
---

### **Hemos pasado de 12 variables con 16 filas cada una ... a 3 variables con 176 filas.**

### **Y pues ya estaríamos listos para empezar a hacer los análisis =)**

Modificar la base de datos original, para ajustarla a tus necesidades a la hora de realizar los análisis, es la parte más complicada en la mayoría de los casos. Con lo cual es una habilidad a tener en cuenta. Algunas bases de datos son más fáciles de preprocesar que otras.

---

## **Comparación de más de dos grupos independientes, ***ANOVA de una vía***, ***ANOVA de Welch*** y ***Kruskal-Wallis***.**  

* Para elegir el modelo idóneo hay que estudiar primero si los datos son ***normales*** y ***homocedásticos***.

  * Test normalidad: **n = 4** (n < 50) ***Shapiro-Wilks*** (en caso de que n > 50: **Kolmogorov Smirnov**)
  * Test para la homogeneidad de varianzas: ***Levene***.

En ese sentido:

---

* ***Cumplimiento de la suposición de la normalidad (p>0.05) y homocedasticidad (p>0.05)***: **ANOVA de una vía**. En presencia de diferencias significativas, realizaremos el **test de Tukey**
  * **Ojo!!!, todos los grupos tienen que ser normales**. Igualmente, he visto algunas personas que estudian la normalidad del conjunto de datos, y no por separado por cada grupo. Pero nosotros los realizaremos así.

* ***Cumplimiento de la suposición de la normalidad (p>0.05), pero no de la homocedasticidad (p<0.05)***: **ANOVA de Welch**, en presencia de diferencias significativas, realizaromos el **test de Games-Howell**. 

---

### ***OJO!!!, me equivoqué con respecto al post-hoc de Games-Howell***

Me enteré de que este test es recomendable que ***n > 6*** para cada uno de los grupos de estudio, y la nuestra como ya hemos dicho es ***n = 4***. Hay un problema y es que ```rstatix``` no tiene los otros post-hoc para varianzas desiguales... lo cual es desafortunado.

**No voy a modificar ni borrar** el procedimiento para hacer este test en el script de inferencia estadística. Ya que es una de las opciones más potentes, te recomiendo que lo utilices siempre, si tu muestra es mayor a la ya mencionada. 

Lo que voy a hacer es aplicar el modelo de **Tamhane T2**, pero no lo voy a hacer en R, sino en ***Python***, ya que me sirve para practicar este lenguaje. Para ello utilizaré la librería ```scikit_posthocs```. El problema de esta, es que no es tan automática como ```rstatix```, pero bueno... la vida es dura. Para ello puedes ver este archivo de [Jupyter Notebook](https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/scripts_codigo/post_hoc.ipynb)

---

* ***Incumplimiento de la suposición de la normalidad (p<0.05)***: **Kruskal-Wallis**. En presencia de diferencias significativas: **test de Dunnet, y como corrección usaremos Bonferroni** (hay otras como Benjamin Hoschberg, Holms...).

Esto anterior, en caso de que lo que comparáramos **dos** grupos independientes:

* ***Cumplimiento de la suposición de la normalidad (p>0.05) y homocedasticidad (p>0.05):*** **T test**. 

* ***Cumplimiento de la suposición de la normalidad (p>0.05), pero no de la homocedasticidad (p<0.05):*** **T de Welch**.

* ***Incumplimiento de la suposición de la normalidad (p<0.05):*** **Wilcoxon** 

--- 

El procedimiento básico para realizar los análisis para cada uno de los test es el mismo, salvo una ligera diferencia entre la prueba de normalidad y la de levene, anova, welch...
* **Integrantes de las funciones**
  - df: nuestra base de datos = ```tidy_artemias```
  - grupo1: variable con los tratamientos = ```tratamiento```
  - grupo2: ácidos grasos = ```acido_graso```
  - variable_numerica: valores numéricos originales de las variables de ácidos grasos: ```valores```. 
  - Vector para filtrar, ya que no cumple las expectativas para realizar alguno de los modelos: ```vector_x```. En este caso filtraremos siempre ácidos grasos. Por ejemplo.

```
# Prueba de la normalidad
normalidad <- df %>%                   # Asignamos a la función como "normalidad"
  group_by(grupo1, grupo2) %>%         # Agrupamos la variable numérica por el tratamiento y el acido_graso
  shapiro_test(variable_numerica)      # Realizamos el test de shapiro wilks con la variable de los valores de cada AG

# Prueba de la homocedasticidad
levene <- df %>%                                        # Asignamos la función como "levene"
  group_by(grupo2) %>%                                  # Agrupamos únicamente por acido_graso
  mutate(grupo1=as.factor(grupo1)) %>%                  # Convertimos la variable tratamiento en un factor (necesario para Levene)
  levene_test(variable_numerica~grupo1, center = mean)  # Realizamos la función de Levene, la ~ es que queremos como funcion esa de ahí, la variable numérica en función de los tratamientos,
                                                        # valores~tratamiento, el center es que digamos que nos de los valores si usamos 
                                                        # la media como valor de centralización (nada que preocuparse xd). 

# Modelo estadístico. El de abajo es ANOVA de una vía, Pero funciona igual con todos. 
modelo_ejemplo_anova <- df %>%            # Asignamos el valor como modelo_anova, modelo_welch...
  filter(!(grupo2 %in% vector_x)) %>%    # Filtramos los valores, en este caso, que no presenten ni normalidad, ni homocedasticidad !() función que sgnifica NO, %in% asignación de un rango
  group_by(grupo2) %>%                    # Agrupamos por ácido graso
  anova_test(variable_numerica ~ grupo1)  # Realizamos ANOVA, funciona igual que Levene, pero sin center=mean

# En presencia de diferencias significativas Realizaríamos Tukey
acidos_anova %>% 
  group_by(acido_graso) %>% 
  tukey_hsd(valores ~ tratamiento)   
```

Enhorabuena, con esto ya deberías ser capaz de hacer todos los análisis. Esto es comentar un poco como es cada cosa, pero en el script está todo hecho, y con algunos comentarios de cada cosa también. Puede que haya pequeñas diferencias entre cada test, como aplicar Bonferroni en ```dunn_test(x~y, p.adjust.method = "bonf")```. Pero la idea básica es esta.

Una cosa interesante, es una vez tenemos los resultados se nos crea en cada caso una tabla con cada uno de estos, así como en SPSS. Pero una cosa muy buena que tiene este ```rstatix```, es que podemos filtrar los datos que nos interesa con ```filter()``` así como seleccionar las variables resultantes que queremos ver, (los ácidos grasos, tratamiento y el p-valor)  con ```select()```. Veremos como sería esto en la prueba de normalidad y levene

```
normalidad <- tidy_artemias %>% 
  group_by(tratamiento, acido_graso) %>% 
  shapiro_test(valores) %>% 
  filter(p < 0.05)  %>%                  # filtramos los p-valores menores a 0.05
  select(tratamiento, acido_graso, p)    # selecionamos las variables que nos tinteresa ver
normalidad

levene <- tidy_artemias %>% 
  group_by(acido_graso) %>% 
  mutate(tratamiento=as.factor(tratamiento)) %>% 
  levene_test(valores~tratamiento, center = mean) %>% 
  filter(p < 0.05) %>% 
  select(acido_graso, p)
levene
```

output

```
> normalidad
# A tibble: 2 x 3
  tratamiento            acido_graso       p
  <chr>                  <chr>         <dbl>
1 Echium/Bacalao         C 18:3n-3   0.00861
2 Enriquecedor comercial C 16:0      0.0497

> levene
# A tibble: 6 x 2
  acido_graso        p
  <chr>          <dbl>
1 C 16:1n-7   0.000102
2 C 18:0      0.00105
3 C 18:1n-9   0.0190
4 C 18:3n-6   0.0243
5 C 20:5n-3   0.0277
6 C 22:6n-3   0.0203
```

### **Con estos valores anteriores no haríamos ANOVA de una Vía (Bueno... si quieres redondear Enriquecedor comercial allá cada uno), con lo que de nuevo con ```filter()``` filtraríamos los ácidos grasos que nos interesen para cada modelo.**

## **Transformación de los datos para convertirlos si podemos en normales:**

Si quieres sí o sí hacer un ANOVA por x razones, y no te salen los datos normales u homocedásticos, lo que puedes hacer es modificar los datos mediante una serie de técnicas, que puedes hacer con ```mutate()```, si no te había quedado claro que hace esta función ahora la entenderás mejor. Creamos nuevas variables modificando ```valores```, que a lo mejor hagan normales u homocedásticos a los datos de interés (mi humilde opinión, una pérdida de tiempo xd).

```
tidy_artemias %>% 
  filter(acido_graso %in% c("C 16:0", "C 16:1n-7","C 18:0", "C 18:1n-9",
                            "C 18:3n-6", "C 20:5n-3", "C 22:6n-3")) %>% 
  mutate(valores_arcoseno = asin(sqrt(valores)/100),  # Técnica del arcoseno de la raíz cuadrada
         valores_cuadrado = valores^2,                # valores al cuadrado
         valores_raiz = sqrt(valores),                # raíz cuadrada
         valores_log = log10(valores),                # logaritmo
         valores_log_mas_1 = log10(valores + 1),      # logaritmo + 1 (Valores negativos)
         valores_inversa = 1/valores                  # valores inversos
         )
```

Output: no se ven todas las columnas, pero espero que pilles la idea.

```
A tibble: 112 x 9
   tratamiento acido_graso valores valores_arcoseno valores_cuadrado
   <chr>       <chr>         <dbl>            <dbl>            <dbl>
 1 Levadura    C 16:0        10.4           0.0323          109.
 2 Levadura    C 16:1n-7      2.45          0.0157            6.00
 3 Levadura    C 18:0         5.49          0.0234           30.1
 4 Levadura    C 18:1n-9     21.0           0.0459          442.
 5 Levadura    C 18:3n-6      0.31          0.00557           0.0961
 6 Levadura    C 20:5n-3      2.07          0.0144            4.28
 7 Levadura    C 22:6n-3      0.01          0.00100           0.0001
 8 Levadura    C 16:0        10.5           0.0324          110.
 9 Levadura    C 16:1n-7      2.46          0.0157            6.05
10 Levadura    C 18:0         5.52          0.0235           30.5
# ... with 102 more rows, and 4 more variables: valores_raiz <dbl>,
#   valores_log <dbl>, valores_log_mas_1 <dbl>, valores_inversa <dbl>
```

## **Parte 2) Análisis de Componentes Principales (PCA): [pca_faa.R](https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/scripts_codigo/pca_faa.R)**.

Para el caso del PCA, al ser un análisis multivariante, esta vez nos interesa más que la base de datos sea ancha antes que larga, con lo que esta vez nos ahorraremos convertirla en *tidy_artemias*. Si te interesa entender un poco la teoría del PCA, en este trabajo no te la voy a contar xd. hay un vídeo de youtube muy bueno que te explica las ideas principales en 5 minutos. También lo hay en versión más amplia y detallada, y otro para hacerlo en R (yo lo hago parecido, pero a mi manera). Es de [Stat Quest](https://www.youtube.com/c/joshstarmer/videos), ¡y el que lo explica es un genético! (sip, si te gusta la genética vete pensando en hacerte con R o Python, en Estados Unidos, en este campo, ya prácticamente es un trámite, y lo que ocurre allí llegará tarde o temprano aquí... viendo lo desfasada que se ha quedado la carrera tiene pinta que tarde un poco):

* [StatQuest, PCA in 5 minutes](https://www.youtube.com/watch?v=HMOI_lkzW08&t=192s&ab_channel=StatQuestwithJoshStarmer)

* [StatQuest, PCA in R](https://www.youtube.com/watch?v=0Jp4gsfOLMs&t=91s&ab_channel=StatQuestwithJoshStarmer)

Lo primero que hay que hacer, es deshacernos por el momento de la variable de los grupos (```tratamiento```). 

###### **Una nota sobre R base** (programación en R con su código original):
* Para un **Data Frame** (es como se le llaman a las tablas que buscan simular a las celdas de excel, propias de este lenguaje), al que le asignamos el nombre de df ```df <- data_frame```, que digamos que presenta un **vector** (conjunto de valores, numéricos, categóricos, lógicos... concatenados en serie, en caso de en contrarse en un data frame estarían formando las columnas/variable) llamado "x", otro "y" y un último llamado "z", podemos seleccionar cada una de las columnas de dos maneras: con el símbolo ```$``` o con corchetes y una "," a la izquierda del nombre o número del vector ```[,]```. Ejemplo con código, juega con el:

```
df <- data.frame(x = c(1:10), y = c(1:10), z = c(1:10))
df
# Seleccionar la variable "x":
df$x
df[,"x"]
# o el número de la columna
df[,1]
# en caso de añadirle un -, no selecionamos, sino que eliminamos una variable (como ```select()``` en dplyr)
df <- df[,-1]
df
# Además para que lo sepas, si añades un número delante de la "," selecionas filas.
df[1,]      # Selecionamos la fíla 1
df[1:4, 2]  # Selecionamos las filas de la 1 a la 4 de la columna 2   
```
En este caso, con la técnica anterior, nos deshacemos de la variable ```tratamiento```:

```
ncol(matrix_artemias)
matrix.artem.num <- matrix_artemias[,-12]
matrix.artem.num
```
output:

```
> matrix.artem.num
# A tibble: 16 x 11
   `C 16:0` `C 16:1n-7` `C 18:0` `C 18:1n-9` `C 18:2n-6` `C 18:3n-6` `C 18:3n-3`
      <dbl>       <dbl>    <dbl>       <dbl>       <dbl>       <dbl>       <dbl>
 1    10.4         2.45     5.49        21.0        6.75        0.31        23.5
 2    10.5         2.46     5.52        20.6        6.55        0.37        24.0
 3    10.5         2.43     5.53        20.4        6.47        0.27        23.7
 4    10.8         2.47     5.66        21.3        6.38        0.26        22.7
 5    12.2         2.31     5.68        18.2        4.01        0.23        19.9
 6    11.8         2.2      5.37        17.4        4.15        0.22        20.7
 7    11.4         2.2      5.18        17.0        4.35        0.25        21.1
 8    11.7         2.27     5.61        17.8        4.25        0.24        20.6
 9    10.0         2.8      4.91        18.7        6.1         0.96        25.1
10    10.1         2.63     4.99        19.0        6.06        0.89        25.1
11     9.96        2.78     4.9         18.6        6.01        0.9         25.1
12    10.0         2.67     5.01        19.0        5.97        0.99        24.5
13    11.4         2.16     5.01        18.2        6.69        0.26        22.2
14    11.4         2.18     5.03        18.2        6.54        0.25        22.2
15    11.4         2.15     5.04        18.1        6.42        0.27        22.0
16    11.3         2.18     5.01        18.4        6.5         0.23        22.1
# ... with 4 more variables: `C 18:4n-3` <dbl>, `C 20:4n-6` <dbl>,
#   `C 20:5n-3` <dbl>, `C 22:6n-3` <dbl>
```
Tenemos ya únicamente las variables numéricas, si no no se puede hacer el PCA.

* Lo siguiente es realizar el pca. 
  - para ello usaremos la función ```prcomp()``` con los siguientes parámetros: ```prcomp(df, scale=TRUE, center=TRUE)```.
  - Lo siguiente será utilizar la función ```summary()```, que nos dará los resultatdos de cada una de las componentes en una serie de parámetros, a nosotros nos interesa la varianza explicada por las primeras componentes (escogeremos las 3 primeras: PC1, PC2 y PC3) y la varianza acumulada.
  - Además, interesa usar *Pearson* para estudiar la correlación lineal entre las variables y las componentes con la función ```cor()```. Nos dará una idea de cómo se van a ordenar los datos una vez los grafiquemos.

```
pca <- prcomp(matrix.artem.num, scale = T, center = T)

# Vamos a ver un resumen del pca para sacar información que viene bien.

resumen <- summary(pca)
resumen$importance[,c("PC1", "PC2", "PC3")]

# Lo interesante de la función de arriba es la proproción de la varianza
# explicada por las primeras componentes.
# La PC1 explica un total del 61.28% de la varianza
# La PC2 explica un total del 22.54% de la varianza
# La PC3 explica un total del 14.14% de la varianza
# Entre las tres explican un total de 97.96% de la varianza acumulada explicada (con que las primeras 
# 2 componentes lleguen al 60% está bien, pero voy a pillar la PC3 por una sorpresilla)

# Correlación (Pearson) entre las variables y las componentes. De aquí se puede 
# sacar información interesante, dependindo de los valores, se encontrarán en una 
# posición u otra del gráfico que haremos

t(cor(matrix.artem.num, pca$x[,c(1,2)]))

```
output:

```
> resumen
Importance of components:
                          PC1    PC2    PC3     
Standard deviation     2.5963 1.5746 1.2472 
Proportion of Variance 0.6128 0.2254 0.1414 
Cumulative Proportion  0.6128 0.8382 0.9796

> t(cor(matrix.artem.num, pca$x[,c(1,2)]))
         C 16:0  C 16:1n-7    C 18:0  C 18:1n-9  C 18:2n-6  C 18:3n-6
PC1  0.97896130 -0.9182935 0.3557374 -0.5509704 -0.5670637 -0.8423759s
PC2 -0.02365045 -0.2093470 0.5480327  0.7903879  0.5578415 -0.5216688
      C 18:3n-3  C 18:4n-3 C 20:4n-6  C 20:5n-3  C 22:6n-3
PC1 -0.97458606 -0.9378363 0.6382211  0.7441699  0.8291693
PC2  0.00288405 -0.3069549 0.6049313 -0.5923490 -0.3401095s
```

Lo siguiente que habría que hacer, es extraer los valores de las 3 primeras componentes y juntarlas con la variable que nos deshicimos al principio, ```tratamiento```, para ello me podemos usar el siguiente código. 

##### **Nota,** ***tibble*** es lo mismo que **data frame**, pero a la hora de dar el output, te lo dá más "estético" y ordenado. Me he acostumbrado a usarlo últimamente, con lo que la función ```as_tibble()``` convierte a un conjunto de valores en este tipo de tabla. Los valores de las componentes es "x" en el pca. 

```
princ_comp <- as_tibble(pca$x[,c(1,2,3)]) %>% 
  mutate(tratamiento=matrix_artemias$tratamiento)
princ_comp
```
output:

```
> princ_comp
# A tibble: 16 x 4
      PC1    PC2      PC3 tratamiento
    <dbl>  <dbl>    <dbl> <chr>
 1 -1.29   2.36   0.442   Levadura
 2 -1.37   2.02   0.472   Levadura
 3 -1.10   2.09   0.537   Levadura
 4 -0.813  2.63   1.23    Levadura
 5  3.09  -0.400  2.15    Lectina marina
 6  3.30  -1.42   0.810   Lectina marina
 7  2.96  -1.75   0.135   Lectina marina
 8  2.99  -0.843  1.51    Lectina marina
 9 -3.75  -1.82  -0.0488  Echium/Bacalao
10 -3.26  -1.28  -0.00116 Echium/Bacalao
11 -3.56  -1.64  -0.0343  Echium/Bacalao
12 -3.30  -1.45   0.158   Echium/Bacalao
13  1.49   0.388 -1.96    Enriquecedor comercial
14  1.44   0.390 -1.76    Enriquecedor comercial
15  1.68   0.278 -1.79    Enriquecedor comercial
16  1.49   0.435 -1.83    Enriquecedor comercial
```

### ***Y ya habríamos terminado la parte técnica del pca, para interpretar estos resultados, hay que graficarlos. Para ello voy a dedicar otra parte únicamente a la visualización de los datos mediante los paquetes de ```ggplot2``` y ```plot_tly```.***

## **Parte 3) visualización de los datos. [visalizacion.R](https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/scripts_codigo/visualizacion.R)** (aunque los gráficos de PCA y RCA están en el script de [pca_faa.R](https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/scripts_codigo/pca_faa.R))

Ahora entramos en la parte más divertida, la de diseñar los gráficos a partir de una serie de librerías. ```ggplot2``` es una de las mejores librerías que te vas a encontrar en este aspecto. Para mí es la mejor, es una de las razones por las que he aprendido R en vez de Python, sin ninguna duda. 

Si es cierto que su sintaxis puede ser un poco más compleja que su contraposición de ```matplotlib``` en Python, pero su combinación con ```dplyr```, le da una plasticidad espectacular a la historia que le puedes dar a tus datos.

Este es un tema candente para muchos en la **ciencia de datos**. Crear un buen gráfico no es fácil y lleva tiempo. No todos son automáticos. 

En mi caso, aprender a diseñar estos gráficos me ha permitido afianzar los conocimientos que tengo de estadística de la carrera. Una vez vas creando por ti mismo el gráfico de un PCA te obliga a entender lo mínimo que está pasando. A ver, no eres estadístico, y no vas a poder explicar la teoría de los eginvalues o distancias euclidianas, pero más o menos vas entendiéndolo de manera muy básica para generar una interpretación.  


**Existen dos tipos de gráficos:**

--- 

* ***Exploratorio***: te permiten ver tus resultados. Sirven para ti mismo y pueden ser en ese sentido más automáticos, ya que para entender tu mismo tus datos no tiene que ser muy elaborado.

* ***Explicativo***: los haces con la intención de resaltar la información importante que puedan aportar. Sobre todo si tu intención es que otras personas lo vean. Lo tienes que hacer visual, limpio, sin excederte tampoco para no desviar la atención a cosas que no sean tan importantes.

---

Comenzaré con las gráficas que hecho sobre inferencia estadística y por último haremos los resultados del PCA. 

Realmente aquí no puedo explicar absolutamente todo de ggplot2, lo que haré será darte los input para crear gráficos básicos y mostraré en lo que se pueden convertir. Tienes mi código para complementarlos como te venga en gana. De hecho, así es como se aprende a programar.

Hay una página que me sirvió mucho en su momento, se llama The R Graph Galery: 

[link Graph Galery](https://r-graph-gallery.com/)

También recomiendo los tutoriales de Rafa Gonzáles Gouviera:

[link tutorales R](https://www.youtube.com/watch?v=zAzpuLJA29U&list=PLbDLkhJ5sFvCWFbP4tAFALHkNWNFo_FiL&index=15&ab_channel=RafaGonzalezGouveia)


Dejar claro que no es mi intención interpretar los resultados. Yo ya lo hice en su momento. Eso te toca a ti. 

## **Gráficos de inferencia estadística:**

Primero graficaremos los gráficos normales y posteriormente los no normales. ¿A qué se debe esto, por qué no lo graficamos todos juntos? .

1) Sería demasiada información en un gráfico.
2) Podríamos graficarlos por seaprado, ya que lo que nos interesa es ver si hay diferencias significativas dependiendo del tratamiento según para cada uno de los ácidos grasos, pero para ello habría que hacer 16 gráficas, inviable.
3) Podríamos buscar alguna forma de juntar los ácidos grasos por algún motivo fisiológico, que sean insaturados, saturados, w-3...
4) **ojo!!** Sin embargo nos vemos en una tesitura estadística. Los test paramétricos (los dos ANOVAS) al ser los datos normales, lo que comparan para ver si los grupos presentan diferencias significativas son las ***medias*** de los grupos y la dispersión de los datos sobre esta, mientras que los test no paramétricos (Kruskal-Wallis) lo que se comparan son las ***medianas*** y la dispersión de los datos frente a esta. 

En este caso en concreto, elegir cómo graficarlos de una manera coherente fisiológicamente hablando no es tan sencillo. O al menos a mí no me lo parece.

Por motivos didácticos, separaré los resultados dependiendo de las circunstancias de la inferencia estadística. Por ello, los resultados normales, los representaremos como barras con la media y su desviación, mientras que para los grupos no normales, los representaremos con un boxplot, para visualizar por otro lado, la mediana y la distribución de los datos.

Si estuviera haciendo un TFG intentaría buscar otra manera de hacerlo. 

### **Gráfico básico de barras** donde se muestra la *media* y la *desviación típica* (para graficar y explicar resultados de ANOVA)

He de decir que estos gráficos, incluso los más básicos a secas, no son tan sencillos, de hecho, tienen sus truquillos.

```
data_frame %>% 
  group_by(variable_categorica1, variable_categorica2) %>%                       # En ocasiones solo haría falta de agrupar con una única variable categórica, pero en este caso agruparíamos con acido_graso y tratamiento
  summarise(media = mean(variable_numerica), desvaicion_tipica=sd(valores)) %>%  # summarise() permite calcular estadísticos resumiendo la información (lo que lo diferencia de mutate()!), al agruparlos, calcula la media y desviación
  ggplot(aes(x = variable_categoria1,y = media, fill=variable_categorica2)) +    # función de ggplot2, hay que poner las variables x, y, fill (color de relleno, ojo! es de hecho importante, si no, no se separan los grupos, pero esto lo entenderás con la experiencia xd)
  geom_bar(stat = "identity", position = position_dodge()) +                     # geometría de barras
  geom_errorbar(aes(ymin=media-sd, ymax=media+sd),                               # geometría de barras de error (media +- la desviación típica), algunas personas que he visto en la carrera que hacen esta gráfica en excel les queda la disviación más grande por un lado de la grafica que por otro, cuidado con eso, debe ser igual en ambos lados
                position = position_dodge(.9))

```

### **Este sería su potencial**

---

<p align="center">
<img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/graficas/Rplot03.png">
</p>

Figura 1.

<p align="center">
<img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/graficas/Rplot04.png">
</p>

Figura 2. 

---

**Diagrama de cajas** (**boxplot**) para ver la *mediana* y la *dispersión de los valores* (cuartiles, bigotes...). Un boxplot básico es más asequible que el de barras.

```
# Boxplot básico
data_frame %>% 
  ggplot(aes(variable_categorica1, variable_numerica, fill=variable_categorica2)) +
  geom_boxplot() 
```

### **Y este ha sido el que yo he hecho.**

---

<p align="center">
     <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/graficas/Rplot06.png">
</p>

Figura 3

---

A estos gráficos se le pueden añadir letras de significación (la ***a***, ***b***...) según la existencia de diferencias (la función para añadir texto sería ```geom_text()```). Aunque a mí, mi tutor de TFG (que era del área de estadística) me recomendó que no me la jugara, por que es difícil usar bien ese tipo de etiquetas y algunos profesores no les gustaba. Lo mejor es siempre explicar las diferencias en la memoria por escrito. Además... son muchos grupos, no me da la vida.

## **Gráfico de PCA**

Hacer uno básico de PCA es sencillo también en verdad. Este sería el input

```
data_frame %>% 
ggplot(aes(PC1, PC2, color = variable_categorica)) +
  geom_point()

```

### Y este sería su potencial

---

<p align="center">
<img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/graficas/Rplot02.png">
</p>

***Figura 4.*** Análisis de componentes principales para distintos tratamientos de diverso contenido de ácidos grasos.

--- 

## ***Aún así, es aconsejable realizar técnicas de rotación, con las componentes que expliquen una mayor varianza y hayamos decidido trabajar con ellas. En este caso PC1 y PC2.***

La idea de rotar las componentes, teóricamente y estadísticamente hablando,  es adquirir un mayor nivel de interpretación de los datos.

Para ello una técnica muy utilizada es ```varimax```, se usaría el siguiente código ultilizando la función ```principal()``` de la librería ```psysch```:

```
library(psych)
rpca <- principal(matrix.artem.num,         # Usamos nuestra base de datos
                  nfactors = 2,             # Selecionamos las dos primeras componentes
                  rotate = "varimax",       # La técnica de rotación que usaremos 
                  scores = TRUE)

```

En ese sentido, al graficar, utilizaríamos los ```scores``` (valores de las componentes 1 y 2 rotadas) que nos proporciona esta función.

Y lo siguiente sería realizar el mismo gráfico de las componentes, pero esta vez con los rotados. Obteniendo como resultado:

---

<p align="center">
<img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/graficas/rpca.png">
</p>

***Figura 5.*** Análisis de componentes principales para distintos tratamientos de diverso contenido de ácidos grasos.

--- 

### **Nota sobre el PCA (rotado o no):** recuerda que para sacar una interpretación biológica de este gráfico, tienes que estudiar las componentes por separado, es decir, PC1 y PC2 son independientes (aunque la que tiene más varianza explicada en principio es la que más peso presenta en el análisis). Otra cosa que deberías hacer y está en el script del pca rotado, es realizar inferencia estadística (ANOVA, kruskal...) para cada componente, para ver si los grupos presentan diferencias significativas. 

---

Como he dicho, no es mi intención dar una interpretación del PCA, pero si necesitas un ejemplo, lo he hecho en mis resultados del [TFG](https://github.com/Juankkar/Tortugas_La_Tahonilla). Hice un PCA con una interpretación muy sencilla, tenía un patrón biológico claro sobre la biometría de las tortugas rescatadas por la Tahonilla. En principio no sé como llevaré ese proyecto; en el momento en el que estoy escribiendo aún no lo he podido poner a punto del todo, habrá algunos fallos (creo que en cuanto a interpretación no debería haberme dejado nada). 

Por último, probé gráficos 3D de otra librería de ```plot_ly```. Podemos añadir en ese sentido una tercera componente (```PC3```) y de esa manera podemos rotar las componentes a nuestro antojo (además aumenta la varianza explicada acumulada!!!), lo cual puede ayudar en la interpretación de los resultados. No voy a explicar cómo hacerlo, porque para cuando estoy haciendo esta memoria lo acabo de descubrir. Igualmente de dejo el Link de la página de Plotly donde aprendí a hacerlo: [Gráfico PCA plotly](https://plotly.com/r/pca-visualization/)

Igualmente, te advierto que este gráfico es una flipada en verdad porque en la práctica ¿adivina qué? Las memorias que hagas son en 2D, con lo cual de nada te sirve una imagen 3D no interactiva en 2D. Es más, te preguntarás por qué no lo he subido aquí, si GitHub permite subir este tipo de gráficos... bueno ... Literal el archivo en HTML es tan grande que no se puede ver aquí. Todos mis spripts de R en este directorio no suman casi ni 1% en comparación con ese archivo, lástima. 


## **Se acabó**
### Espero que te sirva =)
