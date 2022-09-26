###############################################################################
#                         informe de prácticas de GEVO                        #
###############################################################################

# Antes que nada cargaremos las funciones propias que he hecho para estas prácticas...

source("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/GEVO/funciones.R")

#### Si no tienes los paquetes recuerda instalarlos!!!!
# Ejemplo, puedes:
# install.packages("tidyverse")
# ...

library(tidyverse)
library(ggthemes)
library(readxl)
library(rstatix)

# (pchsss...) Lo primero que te recomiendo es que veas si la plantilla que te dan este año es la misma que la que nos dieron a nosotros el año pasado
# en tal caso, si cambias los valores del año pasado por los tuyos, tal como están en el csv de abajo, en teoría deberías poder correr  el script 
# perfectamente, con tus datos :). Sólo hay un pero, cuando hagas la parte de Hardy Weinberg, tienes que asegurarte en caso de que tengas que hacer la
# correción de Yates, te avisaré que tienes que cambiar con un OJO!!! en su momento en ese caso. 

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

#### H-W test para A25

# Creamos la tabla de Hardy weimberg
tabla_hw_a25 <- ale_tot_juntos %>% 
  filter(locus == "A25") %>% 
  select(`1/1`,`1/2`,`2/2`,procedencia) # Fíjate en la tabla para Yates puedes añadir "%>% view()" para ver la tabla

# Extraemos los vectores de la tabla anterior con la función que creamos
# con el data frame no va, tiene que ser un vector.
a25_clase <- vect_hw(tabla_hw_a25, 1, 1, 2, 3)       
a25_canarias <- vect_hw(tabla_hw_a25, 2, 1, 2, 3)

# Usamos la función que nos hace Hardy Weimberg del paquete HardyWeinberg, NICE
HWChisq(a25_clase,cc=.5, verbose=TRUE)    # ns   OJO!!!, cc=... es la corrección de Yates, si tienes que hacerla ponle un .5, si no, 0. Se aplica cuando el 20% de las casillas de los esperados presentan valores menores a 5... creo xd
HWChisq(a25_canarias,cc=0, verbose=TRUE)  # ns

# H-W test para D1
tabla_hw_d1 <- ale_tot_juntos %>% 
  filter(locus == "D1") %>% 
  select(`1/1`,`1/2`,`2/2`)

d1_clase <- vect_hw(tabla_hw_d1, 1, 1, 2, 3)
d1_canarias <- vect_hw(tabla_hw_d1, 2, 1, 2, 3)

HWChisq(d1_clase,cc=0, verbose=TRUE)     # ns
HWChisq(d1_canarias,cc=0, verbose=TRUE)  # ns

# H-W test para FXIIIB
tabla_hw_fxiiib <- ale_tot_juntos %>% 
  filter(locus == "FXIIIB") %>% 
  select(`1/1`,`1/2`,`2/2`)

fx_clase <- vect_hw(tabla_hw_fxiiib, 1, 1, 2, 3)
fx_canarias <- vect_hw(tabla_hw_fxiiib, 2, 1, 2, 3)

HWChisq(fx_clase,cc=0, verbose=TRUE)     # ns
HWChisq(fx_canarias,cc=0, verbose=TRUE)  # ns

# H-W test para APO
tabla_hw_apo <- ale_tot_juntos %>% 
  filter(locus == "APO") %>% 
  select(`1/1`,`1/2`,`2/2`)

apo_clase <- vect_hw(tabla_hw_apo, 1, 1, 2, 3)
apo_canarias <- vect_hw(tabla_hw_apo, 2, 1, 2, 3)

HWChisq(apo_clase,cc=.5, verbose=TRUE)      # ns
HWChisq(apo_canarias,cc=0.5, verbose=TRUE)  # ns

# H-W test para HS3.23
tabla_hw_hs3 <- ale_tot_juntos %>% 
  filter(locus == "HS3.23") %>% 
  select(`1/1`,`1/2`,`2/2`)

hs3_clase <- vect_hw(tabla_hw_hs3, 1, 1, 2, 3)
hs3_canarias <- vect_hw(tabla_hw_hs3, 2, 1, 2, 3)

HWChisq(hs3_clase,cc=.5, verbose=TRUE)      # ns
HWChisq(hs3_canarias,cc=0, verbose=TRUE)    # ns

# H-W test para HS4.65
tabla_hw_hs4 <- ale_tot_juntos %>% 
  filter(locus == "HS4.65") %>% 
  select(`1/1`,`1/2`,`2/2`)

hs4_clase <- vect_hw(tabla_hw_hs4, 1, 1, 2, 3)
hs4_canarias <- vect_hw(tabla_hw_hs4, 2, 1, 2, 3)

HWChisq(hs4_clase,cc=.5, verbose=TRUE)      # ns
HWChisq(hs4_canarias,cc=0.5, verbose=TRUE)  # ns


#------------------------------------------------------------------------#
# Desequilibrio gamético entre los marcadores genéticos. Datos ficticios #
#------------------------------------------------------------------------#

# Como es una movida la tabla que he hecho, voy a intentar exlicarla. tentdrás que modificarla en tu caso.
# No creo que pongan los mismos resultados (tendrás que cambiar N). A lo mejor han cambiado por completo
# la plantilla de un año para otro xd.
deseq.gam <- tibble(
  A25 = c(rep("1/1",3),rep("1/2",3),rep("2/2",3)),
  D1 = rep(c("1/1","1/2","2/2"),3),
  N = c(98,87,9,45,76,43,6,25,12)
) %>% mutate(total=sum(N),
             nA25_1 = case_when(A25 == "1/1" ~ 2,
                                A25 == "1/2" ~ 1,
                                A25 == "2/2" ~ 0),
             nD1_1 = case_when(D1 == "1/1" ~ 2,
                               D1 == "1/2" ~ 1,
                               D1 == "2/2" ~ 0),
             nA25_2 = case_when(A25 == "1/1" ~ 0,
                                A25 == "1/2" ~ 1,
                                A25 == "2/2" ~ 2),
             nD1_2 = case_when(D1 == "1/1" ~ 0,
                               D1 == "1/2" ~ 1,
                               D1 == "2/2" ~ 2),
              fgenoA25_1=(nA25_1*N)/(2*total),
              fgenoA25_2=(nA25_2*N)/(2*total),
              fgenoD1_1=(nD1_1*N)/(2*total),
              fgenoD1_2=(nD1_2*N)/(2*total),
              ) %>% view # Chiquita tabla, para si es porquería de resultado, mi niño xd

# Número de individuos (es importante para hacer automático el script)
N <- sum(deseq.gam$N)

# Frecuencias alélicas de A25
p_A25 <- sum(deseq.gam$fgenoA25_1)
q_A25 <- sum(deseq.gam$fgenoA25_2)

# Frecuencias alélicas de D1
p_D1 <- sum(deseq.gam$fgenoD1_1)
q_D1 <- sum(deseq.gam$fgenoD1_2)


tabla_construida <- data.frame(
  A1A1=c(98,87,9),
  A1A2=c(45,76,43),
  A2A2=c(6,25,12)
)
rownames(tabla_construida) <- c("B1B1","B1B2","B2B2")

tabla_construida

# Chosss bro... esta función es muy nasty, en plan guay, me siento hasta orgulloso.
# La puedes utilizar para cualquier ejercicio, en vez de darle el valor de las tablas individuos...
# de forma "lógica" como he hecho, puedes darle los valores numéricos.
langley(A=tabla_construida[1,1], B=tabla_construida[1,2], 
        C=tabla_construida[2,1], D=tabla_construida[2,2],
        N=N, 
        p1=p_A25, p2=p_D1, q1=q_A25, q2 =q_D1) 
