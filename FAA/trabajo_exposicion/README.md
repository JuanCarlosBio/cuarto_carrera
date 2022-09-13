# Trabajo de exposición: Efecto de la cosanguineidad en la resolución de problemas para *Mus musculus*

Este trabajo consistió en realizar una exposición de un trabajo técnico. En nuestro caso, simulé un experimento con R.

Básicamente consistió en... hipotéticamente hablando... aparear ratones en un nº de generaciones concreto (7 generaciones). Los cuáles medíamos el tiempo que tardaban una serie de ratones cosanguíneos con otros controles que no estaban emparentados y supuestamente sin endogamia.

La idea es que, a medida que los ratones tienen más generaciones de endoganmia, más tardan en cruzar el laberinto.

## Simulación para sacar los datos.

El truco fue usar una función de R llamada ```sample()``` que puede coger valores arleatorios de una serie de intervalos que le asignes. Seguramente se puede hacer de una manera menos tediosa, pero vamos en su momento funcionó perfectamente. Este fue el código:

```
# # Simulación de los tiempos ratones endogámicos
# generacion0 <- sample(245:300, 12, replace = TRUE) # Básicaente en este por ejemplo, da 12 ratones de 245 a 300 segundos con reemplazo
# generacion1 <- sample(300:400, 12, replace = TRUE)
# generacion2 <- sample(330:450, 12, replace = TRUE)
# generacion3 <- sample(400:550, 12, replace = TRUE)
# generacion4 <- sample(470:590, 12, replace = TRUE)
# generacion5 <- sample(490:600, 12, replace = TRUE)
# generacion6 <- sample(500:600, 12, replace = TRUE)
# generacion7 <- sample(500:600, 12, replace = TRUE)
# 
# # Simulación de los ratones control
# controles_0 <- sample(245:300, 12, replace = TRUE)
# controles_1 <- sample(240:295, 12, replace = TRUE)
# controles_2 <- sample(243:303, 12, replace = TRUE)
# controles_3 <- sample(241:310, 12, replace = TRUE)
# controles_4 <- sample(230:300, 12, replace = TRUE)
# controles_5 <- sample(235:310, 12, replace = TRUE)
# controles_6 <- sample(245:290, 12, replace = TRUE)
# controles_7 <- sample(245:300, 12, replace = TRUE)
```


