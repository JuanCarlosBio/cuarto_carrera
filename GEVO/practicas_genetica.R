###############################################################################
#                         informe de prácticas de GEVO                        #
###############################################################################

#### Si no tienes los paquetes recuerda instalarlos!!!!
# Ejemplo, puedes:
# install.packages("tidyverse")
# ...

library(tidyverse)
library(ggthemes)
library(readxl)
library(rstatix)

# (pchsss...Lo primero que te recomiendo es que veas si la plantilla que te dan este año es la misma que la que nos dieron a nosotros el año pasado
# en tal caso, si cambias los valores del año pasado por los tuyos, tal como están en el csv de abajo, en teoría deberías poder correr  el script 
# perfectamente, con tus datos :), menos la última parte de Hardy Weinberg :(, también tienes que comprobar las tablas para usar o no la corrección 
# de Yates, eso va a ser un problema, llegado el momento te diré que tienes que cambiar con algún comentario con "OJO!!!". Así que tampoco te fíes. 
# En el momento que la tabla tenga una columna de más o fila, sorry, tendrás que modificar un par de cosas. Aún así espero que te ayude :).

practicas_gevo <- read_csv("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/GEVO/practicas_gevo.csv")

# write_csv(practicas_gevo, "practicas_gevo.csv")
# Los nombres de las variables se han vuelto locas ya que están duplicadas, para ser más leíbles al ojo humano, los profesores
# en vez de hacel la tabla larga la han repetido, unos marcadores están a la derecha y otros a la izquierda. Habrá que procesar.

df1 <- practicas_gevo[-c(4,8,12),c(1:7)] %>% select(locus=Locus...1, fenotipo=Fenot....2, 
                                                    genotipo=Genot....3, g=G...4, gf=Gf...5, can=CAN...6, canf=CANf...7)
df2 <- practicas_gevo[-c(4,8,12),c(8:14)]%>% select(locus=Locus...8, fenotipo=Fenot....9, genotipo=Genot....10, 
                                                    g=G...11, gf=Gf...12, can=CAN...13, canf=CANf...14)

# Ya lo hemos dividido, ahora, y les hemos asignado los nombres correctamente lo juntamos para que sea un dataframe largo.

# Individuos canarios analizados de la población:
canarios <- 364

# Esta sería la tabla completada del principio, sólo que en vez de estar dividida
# en dos, estan todos los locus en una fila, más fácil para el programa.
df_gevo <- rbind(df1,df2) %>% 
  mutate(locus=c(rep("APO",3), rep("HS3.23",3), rep("HS4.65",3), 
                                              rep("D1",3), rep("A25",3), rep("FXIIIB",3))) %>% 
  group_by(locus) %>% 
  mutate(gf=round(g/sum(g), 2),
         can=round(sum(canarios)*canf, 0)) %>% 
  ungroup()


df_gevo %>% 
  group_by(locus) %>% 
  summarise(sumatorio_genotipos=sum(g))

df_gevo %>% 
  group_by(locus) %>% 
  summarise(sumatorio_fgenotipo = sum(gf))

#------------------------------------------------------------------------------#
#                         Segunda parte alelos absolutos                       #
#------------------------------------------------------------------------------#
colnames(df_gevo)

# nº de Alelos absolutos de la clase
alelos_clase <- df_gevo %>% select(c("locus", "fenotipo","genotipo", "g"))

# Alelos totales de la clase.
ale_tot_clase <- alelos_clase %>% 
  group_by(locus, genotipo) %>% 
  summarise(num_genotipo = sum(g)) %>% 
  pivot_wider(names_from = genotipo, 
              values_from = num_genotipo) %>% 
  mutate(alelo_1 = 2*`1/1` + `1/2`,
         alelo_2 = 2*`2/2` + `1/2`, 
         procedencia=rep("Clase"))

# Alelos totales de canarias
alelos_canarias <- df_gevo %>% select(c("locus", "fenotipo","genotipo", "can"))

ale_tot_canarias <- alelos_canarias %>% 
  group_by(locus, genotipo) %>% 
  summarise(num.alelos = sum(can)) %>% 
  pivot_wider(names_from = genotipo, 
              values_from = num.alelos) %>% 
  mutate(alelo_1 = 2*`1/1` + `1/2`,
         alelo_2 = 2*`2/2` + `1/2`, 
         procedencia=rep("Canarias"))

# Vamos a juntar estas dos tablas

ale_tot_juntos <- rbind(ale_tot_clase, ale_tot_canarias) %>% ungroup()
# Esto sería las tablitas pequeñas de la plantilla, habría que ir mirando
# y rellenándolas. 
ale_tot_juntos


#------------------------------------------------------------------------------#
#                       Tercera parte, tabla de chicuadrdo, ho, he...                  #
#------------------------------------------------------------------------------#

# Frecuencia relativa de la clase 
frec_relativa <- ale_tot_clase %>% 
  select(-procedencia) %>% 
  pivot_longer(-c(locus, `1/1`, `1/2`, `2/2`), names_to = "alelo", values_to = "numero") %>% 
  group_by(locus) %>% 
  mutate(frec_relativa = numero/sum(numero)) %>% 
  ungroup() 
frec_relativa %>% select(locus, frec_relativa, alelo)

# Heterocigosidad observada
frec_relativa %>% 
  select(-numero, -frec_relativa) %>% 
  group_by(locus, alelo) %>% 
  summarise(het_observada=`1/2`/(`1/1`+`1/2`+`2/2`)) %>% 
  pivot_wider(names_from = alelo, values_from = het_observada) %>% 
  select(locus, het.observada=alelo_1)

# Heterocigosidad esperada

# Creamos un función para sacar la heterocigosidad esperada (psss... si 
# tienes que usar de vez en cuando esta función puedes copiar y pegarla
# en otros lados)

he <- function(frec1, frec2) {
  operacion = 1-(frec1^2+frec2^2)
  return(operacion)
}

frec_relativa %>% 
  select(locus,alelo, frec_relativa) %>% 
  pivot_wider(values_from = frec_relativa, names_from = alelo) %>%
  group_by(locus) %>% 
  summarise(het_esperada=he(alelo_1,alelo_2))

# Frecuencia relativa de la canarias 
frec_rel_can <- ale_tot_canarias %>% 
  select(-procedencia) %>% 
  pivot_longer(-c(locus, `1/1`, `1/2`, `2/2`), names_to = "alelo", values_to = "numero") %>% 
  group_by(locus) %>% 
  mutate(frec_relativa = numero/sum(numero)) %>% 
  ungroup() 
frec_rel_can %>% select(locus, frec_relativa, alelo)

# Heterocigosidad observada
frec_rel_can %>% 
  select(-numero, -frec_relativa) %>% 
  group_by(locus, alelo) %>% 
  summarise(het_observada=`1/2`/(`1/1`+`1/2`+`2/2`)) %>% 
  pivot_wider(names_from = alelo, values_from = het_observada) %>% 
  select(locus, het.observada=alelo_1)

# Heterocigosidad esperada

frec_rel_can %>% 
  select(locus,alelo, frec_relativa) %>% 
  pivot_wider(values_from = frec_relativa, names_from = alelo) %>%
  group_by(locus) %>% 
  summarise(het_esperada=he(alelo_1,alelo_2))


#################################
# Aula vs Canarias Contingencia #
#################################

# APO*
ale_tot_juntos %>% 
  filter(locus == "APO") %>% 
  select(-`1/1`,-`1/2`,-`2/2`,-locus,-procedencia) %>% 
  chisq_test(ale_tot_canarias, correct = F)

#-----------------------------------------------------------------------------#
# D1 ns
ale_tot_juntos %>% 
  filter(locus == "D1") %>% 
  select(-`1/1`,-`1/2`,-`2/2`,-locus,-procedencia) %>% 
  chisq_test(ale_tot_canarias, correct = F)

#-----------------------------------------------------------------------------#
# HS3.23 ns
ale_tot_juntos %>% 
  filter(locus == "HS3.23") %>% 
  select(-`1/1`,-`1/2`,-`2/2`,-locus,-procedencia) %>% 
  chisq_test(ale_tot_canarias, correct = F)
#-----------------------------------------------------------------------------#
# A25 ns
ale_tot_juntos %>% 
  filter(locus == "A25") %>% 
  select(-`1/1`,-`1/2`,-`2/2`,-locus,-procedencia) %>% 
  chisq_test(ale_tot_canarias, correct = F)
#-----------------------------------------------------------------------------#
# HS4.65 ns
ale_tot_juntos %>% 
  filter(locus == "HS4.65") %>% 
  select(-`1/1`,-`1/2`,-`2/2`,-locus,-procedencia) %>% 
  chisq_test(ale_tot_canarias, correct = F)
#-----------------------------------------------------------------------------#
# FXIIIB**
ale_tot_juntos %>% 
  filter(locus == "FXIIIB") %>% 
  select(-`1/1`,-`1/2`,-`2/2`,-locus,-procedencia) %>% 
  chisq_test(ale_tot_canarias, correct = F)
#-----------------------------------------------------------------------------#

# La verdad es que acabo de encotrar esta paquete y no está mal, lo que es muy
# poco automática de serie, pensaría en crear una función pero me puede,a fuerza 
# bruta. Igual en un futuro vuelvo y mejoro el código. 

library(HardyWeinberg) 

# H-W test para A25
ale_tot_juntos %>% 
  filter(locus == "A25") %>% 
  select(`1/1`,`1/2`,`2/2`) # Fíjate en la tabla para Yates
a25_clase <- c(MM=2,MN=12,NN=47)       # OJO!!!, esto es lo que tienes que cambiar!!!, No te preocupes por las letras, esas sirven, tienes que cambiar el número de genotipos
a25_canarias <- c(MM=9,MN=95,NN=260)

HWChisq(a25_clase,cc=.5, verbose=TRUE)    # ns   OJO!!!, cc=... es la corrección de Yates, si tienes que hacerla ponle un .5, si no, 0. Se aplica cuando el 20% de las casillas de los esperados presentan valores menores a 5... creo xd
HWChisq(a25_canarias,cc=0, verbose=TRUE)  # ns

# H-W test para D1
ale_tot_juntos %>% 
  filter(locus == "D1") %>% 
  select(`1/1`,`1/2`,`2/2`)
d1_clase <- c(MM=9,MN=23,NN=27)
d1_canarias <- c(MM=37,MN=179,NN=148)

HWChisq(d1_clase,cc=0, verbose=TRUE)     # ns
HWChisq(d1_canarias,cc=0, verbose=TRUE)  # ns

# H-W test para FXIIIB
ale_tot_juntos %>% 
  filter(locus == "FXIIIB") %>% 
  select(`1/1`,`1/2`,`2/2`)
fx_clase <- c(MM=16,MN=24,NN=14)
fx_canarias <- c(MM=55,MN=169,NN=140)

HWChisq(fx_clase,cc=0, verbose=TRUE)     # ns
HWChisq(fx_canarias,cc=0, verbose=TRUE)  # ns

# H-W test para APO
ale_tot_juntos %>% 
  filter(locus == "APO") %>% 
  select(`1/1`,`1/2`,`2/2`)
apo_clase <- c(MM=48,MN=9,NN=2)
apo_canarias <- c(MM=328,MN=35,NN=1)

HWChisq(apo_clase,cc=.5, verbose=TRUE)      # ns
HWChisq(apo_canarias,cc=0.5, verbose=TRUE)  # ns

# H-W test para HS3.23
ale_tot_juntos %>% 
  filter(locus == "HS3.23") %>% 
  select(`1/1`,`1/2`,`2/2`)
hs3_clase <- c(MM=46,MN=13,NN=0)
hs3_canarias <- c(MM=278,MN=79,NN=7)

HWChisq(hs3_clase,cc=.5, verbose=TRUE)      # ns
HWChisq(hs3_canarias,cc=0, verbose=TRUE)    # ns

# H-W test para HS4.65
ale_tot_juntos %>% 
  filter(locus == "HS4.65") %>% 
  select(`1/1`,`1/2`,`2/2`)
hs4_clase <- c(MM=0,MN=5,NN=56)
hs4_canarias <- c(MM=0,MN=15,NN=349)

HWChisq(hs4_clase,cc=.5, verbose=TRUE)      # ns
HWChisq(hs4_canarias,cc=0.5, verbose=TRUE)  # ns


#------------------------------------------------------------------------#
# Desequilibrio gamético entre los marcadores genéticos. Datos ficticios #
#------------------------------------------------------------------------#


deseq.gam <- tibble(
  A25 = c(rep("1/1",3),rep("1/2",3),rep("2/2",3)),
  D1 = rep(c("1/1","1/2","2/2"),3),
  N = c(98,87,9,45,76,43,6,25,12)
)
deseq.gam

tabla_construida <- data.frame(
  A1A1=c(98,87,9),
  A1A2=c(45,76,43),
  A2A2=c(6,25,12)
)
rownames(tabla_construida) <- c("B1B1","B1B2","B2B2")

tabla_construida

# Ni me acuerdo de cómo hacer esta vaina, si es verdad que la parte interesante era
# la primera, este ejercicio se lo sacaron de la manga muy fuerte. Me da una pereza muy 
# fuerte repasarmelo.
