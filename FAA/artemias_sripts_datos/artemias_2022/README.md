# **Resultados de las artemias 2022 de todos los grupos.**

Si eso en algún momento consultaré la bibliografía para encontrar información interesante y sacar alguna conclusión de estos resultados y lo complemento con el trabajo que hice yo en su momento. Pero la verdad es que en el momento que estoy haciendo esto, no tengo todo el tiempo libre que me gustaría.

## Ácidos grasos de estudio

* *Ác. palmítico:* **C 16:0**
* *Ác. palmitoleico* : **C 16:1n-7**
* *Ác. esteráico:* **C 18:0**
* *Ác. oleico:* **C 18:1n-9**
* *Ác. linoleico:* **C 18:2n-6**
* *Ác. gamma linoléico:* **18:3n-6**
* *Ác. linolénico:* **C18:3n-3**
* *Ác. esterearidónico:* **18:4n-3**
* *Ác. araquidónico:* **20:4n-6 (ARA)**
* *Ác. timmodónico:* **20:5n-3 (EPA)**
* *Ác. docosahexanoico:* **22:6n-3 (DAG)**
## * Gráfico con los resultados obtenidos de todos los ácidos grasos.
---

<p align="center">
  <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/FAA/artemias_sripts_datos/artemias_2022/artemias_2022.png">
</p>

---

**Nota importante antes de continuar:** No encontré en tres de los tratamiento rastros del  ác. gamma linoleico, con lo cual, alginos de los test no corren cuando son sólo valores 0. Pero es obvio que existen por un lado diferencias significativas, al menos en uno de los grupos. En este caso, sólo el tratamiento de aceite de echium bacalao presenta este ácido graso.  

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

Resultados
