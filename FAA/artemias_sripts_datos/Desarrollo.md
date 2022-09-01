# **Desarrollo de las prácticas de Informática Artemias Fisiología Animal Aplicada ULL**

---
## Intención: 

*  *Buenas, mi intención con este Markdown es mostrar como replicaría la práctica de informática de artemias de la asignatura de ***Fisiología Animal Aplicada, Grado de Biología, de la Universidad de La Laguna*** con el lenguaje de programación R. Debido a que me pareció de las prácticas más importantes de cuarto por la falta de alguna asignatura de análisis de datos computacionales. Se trata más que nada comentar parte de lo que he hecho en los sripts, donde está todo resuelto y con algún comentario también. Están hechos de tal forma para que puedas copiar dichos scripts, pegarlo en tu ordenador y correrlo directamente (***recuerda descargar las librarías!!!*** --> ```ìnstall.packages("libreria")```)* 

* *He de decir que no me considero una eminencia ni nada por el estilo con esta herramienta. Pero llevo entre 1 año y medio o 2 aprendiendo lo que puede ofrecer para nosotros los biólogos. Habiéndole piellado cierto tranquillo en el proceso*.

* *Para dejar las cosas claras, mi intención no es enseñar R de principio a fin... solamente comentar como he replicado la práctica de la forma en la que he aprendido ***yo*** a hacerlo con código propio. ***Hay miles de videotutoriales en youtuve*** o en la red (***y gratis, no te vuelvas loc@***) que realizan ese cometido de manera brillante (tanto de R como otros como vendría a ser de Python). Algunas librerías que vendría bien haber visto antes serían, en especial: ***dplyr***, ```ggplot2``` o el conjunto de ```tidyverse```. También tener claro los modelos de inferencia y cuándo elegir uno u otro vendría bien. Utilizaremos una librería muy guapa que te permite hacer estos modelos en combinación con dplyr, ```rstatix```. Y obviamente, debes tener R descargado (es ideal que además tengas Rstudio). Saber cosas básicas como descargar paquetes, asignar valores a objetos (que significa "<-")...*

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

## **Parte 1) Script de inferencia estadística: inferencia_estadistica.R**

Comenzaremos con lo básico, las librerías principales a usar. Haré un comentario de cada una.

```
library(tidyverse) # Paquete con las librerías de dplyr (manipulación de las tablas de datos) y ggolot2 (realización de gráficas de calidad)
library(readxl)    # leer csv, xlsx...
library(rstatix)   # Modelos ANOVA, WELCH...
library(glue)      # permite pegar variables asignadas en texto mediante {}
```

Lo siguiente es poner a punto la base de datos. Vamos a correr los datos (```%>%``` es el pipe (tubería), conecta las funciones entre sí compatibles con ```dplyr```) ...  Está pensado para que únicamente con copiar y pegar el script en tu R personal, puedas correr los datos y que obtengas mis resultados (**Reproducibilidad a tope**)

```

matrix_artemias <- read_csv("C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\FA.A\\PracticasFA.A\\artemias_sripts_datos\\matrix_artemias.csv")

matrix %>% print(5)

```

Veamos el output de las 5 primeras filas...

```
matrix_artemias

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

Tenemos 16 filas de valores de un total de 12 variables. 

---

* La primera variable es la categórica, **Tratamiento**. El grupo 1 = Levadura, grupo 2 = Lectina marina, grupo 3 = Aceite de Echium/Bacalao y grupo 4 = Enriquecedor comercial.
* Las otras variables son las numericas de cada uno de los ácidos grasos de interés. Representan realmenta el % de área de dicho AG que es básicamente la cantidad de AG en la muestra. Nada de lo que preocuparse por ahora.

---

Esta conformación de los datos no es del todo ideal para R. Así que comenzaremos con lo básico de Ciencia de Datos, y es poner la base de datos a punto. 

* Lo primero es cambiar los números de tratamiento por cada uno de sus valores categóricos. Para ello usamos la función ```mutate()``` (crea una nueva variable), y dentro de este crearemos una nueva variable usando ```case_when()```, que mediante un concición, usando el operador ```==```, pregunta: ¿es x igual a y?, y si es ciento con ```~``` le decimos que nos lo convierta en un valor que le asignimos.

* Usaremos ```select()```, selecciona variables que le pidamos, o en este caso si la variable tiene un "-" significa que nos deshacemos de esa variable (la variable original de tratamiento)

* Finalmente, una de las funciones que más me han cambiado la vida. A R (y otra clase de Lenguajes de scripting similaes), le gusta más las bases de datos más estrechas (***"tidy"***), antes que anchas como esta. Es decir, se prefiere que presenten menos número de variables (*columnas*), pero más largas en cuanto *filas* (le permite hacer a la máquina los trabajos de manera más automática). Para ello usaremos ```pivot_longer()```. La idea es pasar de 12 variables a únicamente 3, ¿Cómo hace eso esta función?... suena como romper las leyes de la termodinámica..... es Magic.... Nah es broma. Ahora en serio, vamos a hacer que las columnas de tratamiento y los valores numéricos de las variables se coloquen encima una de las otras de forma ordenada (no te preocupes, R no se inventa nada), mientras que vamos a hacer que los nombres de cada variable numérica (los AG) se conviertan en la tercera columna. Llamaremos a la variable donde están los valores numéricos ***"valores"*** y la que presenta los AG ***"acido_graso"*** (***como consejo***, cuando llames a una variable, hazlo en minúscula sin tildes, y la separación con "_" y evita la ñ espa**ñ**olete). Como **tratamiento** no nos interesa que se modifique, sino que se haga más *larga* únicamente con ella misma, le añadimos un "-".


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

### **Hemos pasado de 12 variabes con 16 filas cada una de esta ... a 3 variables con 176 filas.**

### **Y pues ya estaríamos listos para empezar a hacer los análisis =)**

Modificar la dataset inicial, para ajustarla a tus necesidades a la hora de realizar los análisis, es la parte más complicada en la mayoría de los casos. Con lo cual es una habilidad a tener en cuenta. Algunas datasets son más fáciles de preprocesar que otras.

---

## **Comparación de más de dos grupos independientes, ***ANOVA de una vía***, ***ANOVA de Welch*** y ***Kruskal-Wallis***.**  

Para elegir el modelo idóneo hay que estudiar primero si los datos son ***normales*** (en este caso la n = 4 para cada grupo, con lo que el test utilizado será **Shapiro-Wilks**. En caso de que n > 50 habría que usar Kolmogorov-Smirnov) y ***homocedásticos*** (Usaremos para ello la prueba de **Levene**). En ese sentido:

---

* ***Cumplimiento de la suposición de la normalidad (p>0.05, **ojo!!!, todos los grupos tienen que ser normales**.*** Igualmente, he visto algunas personas que estudian la normalidad del conjunto de datos, y no por separado por cada grupo. Pero nosotros los realizaremos así) y ***homocedasticidad (p>0.05)***: **ANOVA de una vía**. En presencia de diferencias significativas, realizaremos el **test de Tukey**

* ***Cumplimiento de la suposición de la normalidad (p>0.05), pero no de la homocedasticidad (p<0.05)***: **ANOVA de Welch**, en presencia de diferencias significativas, realizaromos el **test de Games-Howell**

* ***Incumplimiento de la suposición de la normalidad (p<0.05)***: **Kruskal-Wallis**. En presencia de diferencias significativas: **test de Dunnet, y como corrección usaremos Bonferroni** (hay otras como Benjamin Hoschberg, Holms...).

Esto anterior, en caso de que lo que comparáramos **dos** grupos independientes:

* ***Cumplimiento de la suposición de la normalidad (p>0.05) y homocedasticidad (p>0.05):*** **T test**. 

* ***Cumplimiento de la suposición de la normalidad (p>0.05), pero no de la homocedasticidad (p<0.05):*** **T de Welch**.

* ***Incumplimiento de la suposición de la normalidad (p<0.05):*** **Wilcoxon** 

--- 

El procedimiento básico para realizar los análisis para cada uno de los test es el mismo, salvo una ligera diferencia entre la prueba de normalidad y la de levene, anova, welch...
* **Integrantes de las funciones**
  - df: nuestra base de datos = *tidy_artemias*
  - grupo1: variable con los tratamientos = *tratamiento*
  - grupo2: ácidos grasos = *acido_graso*
  - variable_numerica: valores numéricos originales de las variables de ácidos grásos: *valores*. 
  - Vector para filtrar, ya que no cumple las expectativas para realizar alguno de los modelos: *vector_x*. En este caso filtraremos siempre ácidos grasos. Por ejemplo.

```
# Prueba de la normalidad
normalidad <- df %>%                   # Asignamos a la función como "normalidad"
  group_by(grupo1, grupo2) %>%         # Agrupamos la variable numérica por el tratamiento y el acido_graso
  shapiro_test(variable_numerica)      # Realizamos el test de shapiro wilks con la variable de los valores de cada AG

# Prueba de la homocedasticidad
levene <- df %>%                                        # Asignamos la función como "levene"
  group_by(grupo2) %>%                                  # Agrupamos únicamente por acido_graso
  mutate(grupo1=as.factor(grupo1)) %>%                  # Convertimos la variable tratamiento en un factor (necesario para Levene)
  levene_test(variable_numerica~grupo1, center = mean)  # Realizamos la función de Levene, la ~ es que queremos como funcion esa de ahí,
                                                        # valores~tratamiento, el center es que digamos que nos de los valores si usamos 
                                                        # la media como valor de centralización (nada que preocuparse xd). 

# Modelo estadístico. El de abajo es ANOVA de una vía, Pero funciona igual con todos. 
modelo_ejemplo_anova <- df %>%            # Asinamos el valor como modelo_anova, modelo_welch...
  filter(!(grupo2 %in% vector_x))) %>%    # Filtramos los valores, en este caso, que no presenten ni normalidad, ni homocedasticidad !() función que sgnifica NO, %in% asignación de un rango
  group_by(grupo2) %>%                    # Agrupamos por ácido graso
  anova_test(variable_numerica ~ grupo1)  # Realizamos ANOVA, funciona igual que Levene, pero sin center=mean

# En presencia de diferencias significativas Realizaríamos Tukey
acidos_anova %>% 
  group_by(acido_graso) %>% 
  tukey_hsd(valores ~ tratamiento)   
```

Enhorabuena, con esto ya deberías ser capaz de hacer todos los análisis. Esto es comentar un poco como es cada cosa, pero en el script está todo hecho, y con algunos comentarios de cada cosa también. Puede que hayan pequeñas diferencias entre cada test, como aplicar Bonferroni en ```Kurskal_test()```. Pero la idea básica es esta.

Una cosa interesante además, es una vez tenemos los resultados se nos crea en cada caso una tabla con cada uno de los reultados, así como en SPSS. Pero una cosa muy buena que tiene este ```rstatix```, es que podemos filtrar los datos que nos interesa con ```**filter()**``` así como seleccionar las variables resultantes que quermos ver, (los ácidos grasos, tratamiento y el p-valor)  con ```**select()**```. Veremos como sería esto en la prueba de normalidad y levene

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

Y esto sería la primera parte realmente, pero me gustaría añadir la transformación de los datos. Si quieres sí o sí hacer un ANOVA por x razones, y no te salen los datos normales u homocedásticos, lo que puedes hacer es modificar los datos mediante una serie de técnicas, que puedes hacer con ```mutate()```, si no te había quedado claro que hacer esta función ahora la verás más clara. Crearemos nuevas variables, que a lo mejor hagan normales o homocedásticas a los datos de interés (variable *valores).

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

output: no se ven todas las columnas, pero espero que pilles la idea. Además, obviamente sería conveniente usar ```filter()``` antes para los ácidos grasos que no sea ni normales ni homocedásticos.

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

## **Parte 2) Análisis de Componentes Principales (PCA): pca_faa.R**.

Para el caso del PCA, al ser un análisis multivariante, esta vez nos interesa más que la base de datos sea ancha antes que larga, con lo que esta vez nos ahorraremos convertirla en *tidy_artemias*. Si te interesa entender un poco la teoría del PCA, en este trabajo no te la voy a contar xd, hay un vídeo de youtube muy bueno que te explica las ideas principales en 5 minutos, también lo hay en versión más amplia y detallada, y otro para hacerlo en R (yo lo hago parecido, pero a mi manera), es de Stat quest, y el que lo explica es un genético (sip, si te gusta la genética vete pensando en hacerte con R o Python):

* [StatQuest, PCA in 5 minutes](https://www.youtube.com/watch?v=HMOI_lkzW08&t=192s&ab_channel=StatQuestwithJoshStarmer)

* [StatQuest, PCA in R](https://www.youtube.com/watch?v=0Jp4gsfOLMs&t=91s&ab_channel=StatQuestwithJoshStarmer)

Lo primero que hay que hacer, es deshacernos por el momento de la variable de los grupos (```tratamiento```). **Una nota sobre R base** (programación en R con su código original): para un data frame (es como se le llaman a las tablas que buscan simular a las celdas de excel, propias de este lenguaje), al que le asignamos el nombre de df ( df <- data_frame), que digamos que presenta un vector llamado "x", otro "y" y un último llamado "z" podemos seleccionar cada uno de ellos de dos maneras: con el símbolo "$" o con corchetes y una como a la izquierda del nombre o número del vector [,]. Ejemplo con código, juega con el:

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
En ese caso haremos lo anterior con tratamiento:

```
ncol(matrix_artemias)
matrix.artem.num <- matrix_artemias[,-12]
matrix.artem.num
```
output:

```
> matrix.artem.num
# A tibble: 16 x 11
   Tratamiento `C 16:0` `C 16:1n-7` `C 18:0` `C 18:1n-9` `C 18:2n-6` `C 18:3n-6`
         <dbl>    <dbl>       <dbl>    <dbl>       <dbl>       <dbl>       <dbl>
 1           1    10.4         2.45     5.49        21.0        6.75        0.31
 2           1    10.5         2.46     5.52        20.6        6.55        0.37
 3           1    10.5         2.43     5.53        20.4        6.47        0.27
 4           1    10.8         2.47     5.66        21.3        6.38        0.26
 5           2    12.2         2.31     5.68        18.2        4.01        0.23
 6           2    11.8         2.2      5.37        17.4        4.15        0.22
 7           2    11.4         2.2      5.18        17.0        4.35        0.25
 8           2    11.7         2.27     5.61        17.8        4.25        0.24
 9           3    10.0         2.8      4.91        18.7        6.1         0.96
10           3    10.1         2.63     4.99        19.0        6.06        0.89
11           3     9.96        2.78     4.9         18.6        6.01        0.9
12           3    10.0         2.67     5.01        19.0        5.97        0.99
13           4    11.4         2.16     5.01        18.2        6.69        0.26
14           4    11.4         2.18     5.03        18.2        6.54        0.25
15           4    11.4         2.15     5.04        18.1        6.42        0.27
16           4    11.3         2.18     5.01        18.4        6.5         0.23
# ... with 4 more variables: `C 18:3n-3` <dbl>, `C 18:4n-3` <dbl>,
#   `C 20:4n-6` <dbl>, `C 20:5n-3` <dbl>
```
Tenemos ya únicamente las variables numéricas, si no no se puede hacer el PCA.

* Lo siguiente es realizar el pca. 
  - para ello usaremos la función realizamos con los siguientes parámetros: ```prcomp(df, scale=TRUE, center=TRUE)```.
  - Lo siguiente será utilizar la función ```summary()```, que nos dará los resutltdos de cada una de las componentes en una serie de parámetros, a nosotros nos interesa la varianza explicada por las primeras componentes (escogeremos las 3 primeras: PC1, PC2 y PC3) y la varianza acumulada.
  - Además interesa usar *Pearson* para estudiar la correlación lineal entre las variables y las componentes con la función ```cor()```. Nos dará una idea de cómo se van a ordenar los datos una vez los grafiquemos.

```
pca <- prcomp(matrix.artem.num, scale = T, center = T)

# Vamos a ver un resumen del pca para sacar información que viene bien.

resumen <- summary(pca)
resumen$importance[,c("PC1", "PC2", "PC3")]

# Lo interesante de la función de abajo es la proproción de la varianza
# Explicada por las primeras componentes.
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
> summary(pca)
Importance of components:
                          PC1    PC2    PC3    
Standard deviation     2.4703 1.6216 1.3981 
Proportion of Variance 0.5547 0.2391 0.1777 
Cumulative Proportion  0.5547 0.7938 0.9715 

> t(cor(matrix.artem.num, pca$x[,c(1,2)]))
    Tratamiento     C 16:0   C 16:1n-7     C 18:0 C 18:1n-9 C 18:2n-6
PC1  0.03874417 -0.9724542  0.90511630 -0.4544657 0.4677226 0.5692914
PC2 -0.72268663 -0.1228565 -0.01096018  0.7197490 0.8765420 0.3815882
     C 18:3n-6  C 18:3n-3  C 18:4n-3  C 20:4n-6  C 20:5n-3
PC1  0.8774424 0.97998403  0.9650077 -0.6465118 -0.7120432
PC2 -0.4112061 0.05535555 -0.2300094  0.3723965 -0.5443734

```

Lo siguiente que habría que hacer, es extraer los valores de las 3 primeras componentes y juntarlas con la variable que nos deshicimos al principio, ```tratamiento```, para ello se me podemos usar el siguiente código. 

##### **Nota,** ***tibble*** es lo mismo que data frame, pero a la hora de dar el output, te lo dá más bonito básicamente y ordenado, y me he acostrumbrado a usarlo últimente, con lo que la función ```as_tibble()``` convierte a un conjunto de valores en este tipo de tabla. los valores de las componentes es "x" en el pca. 

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

### ***Y ya habríamos terminado la parte técnica del pca, para interpretar estos reultados, hay que graficarlos, para ello. Para ello voy a dedicar otra parte únicamente a la visualización de los datos mediante los paquetes de ```ggplot2``` y ```plot_tly```.***

## **Parte 3) visualisación de los datos. visalizacion.R**

Ahora entramos en la parte más divertida, la de diseñar los gráficos a partir de una serie de librerías. ```ggplot2``` es una de las mejores librerías que te vas a encontrar en este aspecto. Para mí es la mejor, es una de las razones por las que he aprendido R en vez de Python sin ninguna duda. 

Si es cierto que su sintaxis puede ser un poco más compleja que su contraposición de ```matplotlib``` en Python, pero su combinación con ```dplyr```, le da una plasticidad espectacular a la historia que le puedes dar a tus datos.

Este es un tema candente para muchos en la Ciencia de Datos. Crear un buen gráfico no es fácil y lleva tiempo. No todos son automáticos, tienes que dedicarles tiempo. 

En mi caso, aprender a diseñar estos gráficos me ha permitido afianzar los conocimientos que tengo de estadística de la carrera. Una vez vas creando por tí mismo el gráfico de un PCA te obliga a entender lo mínicmo que está pasando. A ver, no eres estadístico, y no vas a poder explicar la teoría de los eginvalues o distancia euclidianas, pero más o menos vas entendiéndolo de manera muy básica para generar una interpretación.  

---

**Existen dos tipos de gráficos:**
--- 

* ***Exploratorio***: te permiten ver tus resultados. Sirven para tí mismo, pueden ser en ese sentido más automáticos, ya que para entender tu mismo tus datos no tiene que ser muy elaborado.

* ***Explicativo***: los haces con la intención de resaltar la información importante que puedan aportar. Sobre todo si tu intención es que otras personas lo vean. Lo tienes que hacer visual, limpio sin excecederte tampoco para no desviar la atención a cosas que no sean tan impotantes.

---

Comenzaré con las gráficas que hecho sobre inferencia estadística y por último haremos los reultados del PCA. 

Realmente aquí no puedo explicar absolutamente todo de ggplot2, lo que haré será darte los input para crear gráficos básicos y mostraré en lo que se pueden convertir. Tienes mi código para complementarlos como te venga en gana.

Hay una página que me sirvió mucho en su momento, se llama The R Graph Galery: 

[link Graph Galery](https://r-graph-gallery.com/)

También recomiendo los tutoriales de Rafa Gozáles Gouviera:

[link tutorales R](https://www.youtube.com/watch?v=zAzpuLJA29U&list=PLbDLkhJ5sFvCWFbP4tAFALHkNWNFo_FiL&index=15&ab_channel=RafaGonzalezGouveia)


Dejar claro que no es mi intención interpretar los resultados. Yo ya lo hice en su momento. Eso te toca a tí.

## **Gráficos de inferencia estadística:**

Primero graficaremos los gráficos normales y posteriormente los no normales. ¿A qué se debe esto, por qué no lo graficamos todos juntos? .

1) Sería demasiada impormación en un gráfico.
2) Podríamos graficarlos por seaprados ya que lo que nos interesa es ver si hay diferencias significativas dependiendo del tratamiento según para cada uno de los ácidos grasos, pero para ello habría que hacer 16 gráficas, inviable.
3) Podríamos buscar alguna forma de juntar los ácidos grasos por algún motivo fisiológico, que sean insaturados, saturados, w-3...
4) **ojo!!** Sin embargo nos vemos en una tesitura estadística. Los test paramétricos (los dos ANOVAS) al ser los datos normales, lo que compara para ver si los grupos son significativos es la ***media*** con respecto a la distribución de los datos, mientras que los test no paramétricos (Kruskal-Wallis) lo que se comparan las ***medianas*** con respecto la distribución de los datos. 

Sinceramente, creo que en este caso en concreto, si estuviera haciendo un TFG, al haber tantos grupo, creo que estaría bien tenerlos en un único gráfico, no me volvería muy loco y haría un gráfico de barras con la media y la desviación típica. Pero, por motivos didácticos, separaré los resultados dependiendo de las circunstancias de la inferencia estadística. Por ello, los resultados de ANOVA, los representaremos como barras con la media y su desviación, mientras que para los grupos no normales los representaremos con un boxplot, para visualizar por otro lado, la mediana y la distribución de los datos.

**Gráfico básico de barras** donde se muestra la *media* y la *desviación típica* (para graficar y explicar resultados de ANOVA)

He de decir que estos gráficos incluso el básico persé no son tan sencillos de hecho. Tienen sus truquillos.

```
data_frame %>% 
  group_by(variable_categorica1, variable_categorica2) %>%                       # En ocasiones solo haría falta de agrupar con 
  summarise(media = mean(variable_numerica), desvaicion_tipica=sd(valores)) %>%  # summarise() permite calcular estadísticos, al agruparlos, calcula la media y desviación
  ggplot(aes(x = variable_categoria1,y = media, fill=variable_categorica2)) +    # función de ggplot2, hay que poner las variables x, y, fill (color de relleno)
  geom_bar(stat = "identity", position = position_dodge()) +                     # geometría de barras
  geom_errorbar(aes(ymin=media-sd, ymax=media+sd),                               # geometría de barras de error (media +- la desviación típica), algunas personas que he visto en la carrera que hacen esta gráfica en excel les queda la disviación más grande por un lado de la grafica que por otro, cuidado con eso, debe ser igual en ambos lados
                position = position_dodge(.9))

```

### **Este es su potencial**

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

Para los gráficos de los Ácidos grasos no normales pensé hacer un **diagrama de cajas** (**boxplot**) para ver la *mediana* y la *dispersión de los valores* (cuartiles, bigotes...). Un boxplot es más asequible que el de barras curiosamente

```
# Boxplot básico
data_frame %>% 
  ggplot(aes(variable_categorica1, variable_numerica, fill=variable_categorica2)) +
  geom_boxplot() 
```

### Y esta ha sido el que yo he hecho.

---

<p align="center">
     <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/graficas/Rplot06.png">
</p>

Figura 3

---

A estos gráficos se le pueden añadir letras de significación (la ***a***, ***b***...) según la existencia de diferencias. Aunque a mí, mi tutor de TFG (que era del área de estadística) me reconmendó que no me la juagara, por eue es difícil usar bien ese tipo de etiquetas y algunos profesores no les gustaba. Lo mejor es siempre explicar las diferencias en la memoria.

## Gráfico de PCA

Hacer uno básico de PCA es sencillito en verdad. Este sería el input

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

### **Nota sobre el PCA:** recuerda que para sacar una interpretación biológica de este gráfico, tienes que estudiar las componentes por separado, es decir, PC1 y PC2 son independientes. Otra cosa que deberías hacer y está en el script del pca, es realizar infenrencia estadística (ANOVA, kruskal...) para cada componente, para ver si los grupos presentan diferencias significativas. 

Como he dicho, no es mi intención dar una interpretación del PCA, pero si necesitas un ejemplo, lo he hecho en mis resultados del TFG. Hice un PCA con una interpretación muy sencilla, tenía un patrón biológico claro sobre la biometría de las tortugas rescatadas por la Tahonilla. 

En principio no sé como llevaré ese proyecto; en el momento en el que estoy escribiendo aún no lo he podido poner a punto del todo, habrán algunos fallos (creo que en cuanto a interpretación no debería haberme dejado nada). 

El problema del PCA y otro tipo de técnicas de ordenación multivariante (PCoA, NMDS), es que tú tienes que sacar la interpretación biológica. En ese sentido básicamente el PCA, dicho de una manera muy burda, es coger toda la información que ofrecen las variables, que forman tantas dimensiones como número de variables que puedes comparar, y hacer un mejunje que explique la mayor varianza posible en las primeras componentes, para con suerte, ser capaz de de reducir de x dimensiones a únicamente 2 y poder graficarlas en un paper. En ese momento te verás en la tesitura de pensar si los patrones que ves son verdad o tu cabeza te ha mentido, cosa que no sabrás hasta que se lo expliques a otra persona y arriesgándote a que te mire como si estuviras loco.

Por último, probé gráficos 3D de otra librería de ```plotly```. Podemos añadir en ese sentido una tercera componente (**PC3**) y de esa manera podemos rotar las componentes a nuestro antojo (además aumenta la varianza explicada acumulada!!!), lo cuál puede ayudar en la interpretación de los resultados. No voy a explicar cómo hacerlo, porque para cuando estoy haciendo esta memoria lo acabo de descubrir. Tengo suficiente bagaje para aprender a hacerlo en el momento, pero aún así hay varias cosas que asimilar. Igualmente de dejo el Link de la página de Plotly donde aprendí a hacerlo: [Gráfico PCA plotly](https://plotly.com/r/pca-visualization/)

Igualmente te advierto que este gráfico es una flipada en verdad porque en la práctica ¿adivina que? Las memorias que hagas son en 2D, con lo cual de nada te sirve una imagen 3D no interactiva en 2D. Es más te preguntarás por que no lo he subido aquí si GitHub permite subir este tipo de gráficos... bueno ... literal el archivo en HTML es tan grande que no se puede ver aquí, todos mis spripts de R en este directorio no suman casi ni 1% en comparación con ese archivo. Lástima, hay otras librerías que hacen este cometido como ```rgl```, y puedes conseguir un gif de la imagen en 3D con un peso infinitamente menor, pero no es de tanta calidad como plotly. 


### **It si over**
Espero que te siva =)
