# Prácticas de ABBM 2022.

## Estas son las prácticas de laboratorio de ABBM. Los alumnos compartimos por Whatshapp los resultados de algunos de los grupos (no me acuerdo de si todos).

Los he recopilados y he hecho una serie de análisis en conjunto de todos los datos.

En proceso. Me está costando recordar la parte de cinética. Muy desordenado lo que tengo.

## Resultados

---

<p align="center">
    <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/ABBM/graficas/graficax.png">
</p>

* **Figura 1:**
    * No voy a mentir, no sé muy bien que es esto xd, así que no me hagas mucho caso, en principio es una proteína formada por *Pichia pastoris*. 

    * En ese entonces mediríamos la cantidad de proteínas que forma esta levadura a distintos tiempos, alimentándola de un sustrato en concreto (ni me acuerdo, ONPG fue el que usamos para la última práctica, la 6).

    * Lo lógico sería imagino que la cantidad de proteína formada fuera mayor cada hora que pase.

    * Se usaron dos tipos de disoluciones, 10 ul de extracto de proteína, y 20 ul de extracto por otro lado.

    * Se estudió la absorbancia.

    * Parece no haber modificación significativa en la proteína formada a las 1 hora, 3 horas y 5 horas para los 20 ul. Por otro lado los 10 ul es más raro aún, a las 3 horas hay menos cantidad de proteína que en la primera. Por otro lado, a las 5 horas hay más que a las 3, pero parece haber menos que a las 1.

    * Sin embargo después de hacer inferencia estadística, en todos los casos para cada una de las muestras según su disolución, fueron no significativas estadísticamente.

    * Esto puede deberse a que trabajamos con seres vivos, en este caso las levaduras no siempre trabajan igual, una vez trabajan más, otras menos. También, como estos son los resultados de todos los grupos juntos, pordría ser que se modificara ligeramente el procedimiento de la práctica para cada grupo.    

---

---

<p align="center">
    <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/ABBM/graficas/interpolacion.png">
</p>

* **Figura 2:** 

    * Cada alumno realizó una recta patrón, para distintas concentraciones de albúmnia, de las cuales se conocían los ug de proteína que se le había añadido para ello. 

    * En las bases de datos creo que tengo que el sustrato es ONPG, no hagas caso, es albúmina, solo que esta parte está un poco desastre. En ese sentido, se realizó la absorbancia de 7 tubos + el blanco (=0). 

    * Los 5 primeros fueron para obtener la recta patrón, los 2 últimos (tubos 6 y 7), eran las mestras problamas, el primero era 10 ul de extracto de proteína problema, y el segundo 20 ul.

    * Después de obtener todas las absorbancias de los tubos con albúmina, se realizaron las rectas patrones. En el gráfico se muestran las como líneas grises las de <span style = 'color: darkgray'>**todos los alumnos**</span>. Y en contraste de color rojo <span style= 'color: red'>**uno de los alumnos con mejor recta patrón**</span>, con un coeficiente de correlación lineal de pearson de 0.99 (el mío fue de 0.95 **);**, no está mal tampoco). No fue el único con este valor, pero fue el afortunado de echarlo a suerte con ```sample()```, ¿he dejado claro me encanta esta función xd?.

    *  Luego, obtenidos los resultados de la absorbancia de los tubos problema 6 y 7, se interpoló para sacar los ug de proteína, y sacamos de esta manera la concentración de cada una (no estoy muy seguro si para el segundo caso, el de los 20 ul se multiplica o se divide, en principio lo que yo he hecho, si este tubo tiene más absorbancia sería lógico)

    * Está feo que yo lo diga, pero me ha quedado un gráfica espectacular.

    * Posteriormente se creó una fucnicón para obtener la actividad específica.De manera que añadiendo a la función cada cosa daría el resultado que queremos. **No sé si en el script lo que hice está bien xd**. O sea la función sí, el problema como ocurre siempre con química-bioquímica-bio. molecular... son siempre el problema de las unidades (que ansiedad me da siempre xd). En el exámen de prácticas eso es una movida, también te digo que es a papel y boli, así que la función de abajo no te va a ayudar en ese momento. Añado el código, (si consigues tener cada cosa con sus unidades bien, te debería *funcionar* wonderfully siempre):

```

# Abs: Absotbancia
# vT: Volumane total
# E: Coeficiente de extinción molar
# conc_prot: concentración de la proteína de estudio
# ve: volumen de extracto de la proteína
# t: tiempo

act_esp <- function(Abs, vT, E, conc_prot, ve, t){
  cuerpo=(Abs*vT)/(E*conc_prot*ve*t)
  return(cuerpo)
}
```

---


---

## **Fotografías de los geles (espero que estén bien ordenados)**

* ### **Fotografía Gel 1 (que guapo el paint)**

<p align="center">
    <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/ABBM/geles/Gel1.jpeg">
</p>


* ### **Fotografía Gel 2**

<p align="center">
    <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/ABBM/geles/Gel2.jpeg">
</p>

* ### **Fotografía Gel 3**

<p align="center">
    <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/ABBM/geles/Gel3.jpeg">
</p>

* ### **Fotografía Gel 4**

<p align="center">
    <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/ABBM/geles/Gel4.jpeg">
</p>

## ***Resultado de los geles:***

<p align="center">
    <img src="https://github.com/Juankkar/cuarto_carrera/blob/main/ABBM/graficas/rf.png">
</p>

* **Antes que nada: función para obtener el Rf (movilidad relativa)**. Es bastante sencillita, pero muy útil para utilizarla junto a ```mutate()```

```
rf <- function(dist_frente, dist_prot){
  cuerpo = (dist_prot)/dist_frente
  return(cuerpo)
}
```

* **Figura 3**.
 
    * Identificación de una proteína heteróloga a partir de su movilidad relativa.

    * Primero se hace una una cormatografía de columna, para obtener de un extracto la proteína problema deseada (creo que enconcreto la hicmos de afinidad o intercambio iónico, no me hagas mucho caso, la hice hace mucho tiempo).
    
    * Después la muestra de interés se esudió en un gel de poliacrilamida. Las muestras de todos los alumnos estaban en uno o dos. Yo logré salvar **cuatro geles** que conpartimos los alumnos de mi curso. 
    
    * He de decir que los resultados de las fotografías estaban ahí ahí xd. Intenté sacar como pude en ese sentido la movididad relativa de los geles. En algunos igual he tenido exceso de fe para ver el frente. 

    * El resultado están en esta gráfica para cada uno. Se interpola el **log(PM)**, ojo, no directamente los **Kd**. Los pesos del marcador molecular con el que haces la recta te los dá la profesora.

    * Luego se supone que tienes que buscar la porteína en una base de datos según su peso. 

    * Una cosa que no me gusta del todo, es que en los apuntes pone hacer la gráfica con la ecuación de la recta que es lo que hice. Pero creo que es más correcto trazar una línea que se ajuste correctamente a los puntos en sí. No me pagan lo suficiente...

---