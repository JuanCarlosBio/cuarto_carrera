################################################################################
######################## Prácticas de ecofisiología ############################
################################################################################

library(tidyverse)
library(xlsx)
########## 1) de Prunus lusitanica, interpolarización entre el ETR trans_
##########    porte de electrones, y la luz en el eje x.


# Vectores para el calculo del ETR medio para 3 mediciones (3 hojas distintas):

# Vectores para el cálculo de la media de la luz de 3 mediciones --> 
vector_1 <- c(13,51,106,160,223,345,486,771,1165)
vector_2 <-  c(4,42,94,145,206,323,460,732,1133)
vector_3 <- c(2,38,88,132,192,303,434,702,1080)

vector_tot_L <- vector_1 + vector_2 + vector_3 # Esto sirve como aprendizaje, al sumar vectores, se suman linealmente, el de la posicion 1 se suma con el 1 del otro, hasta el final 
vect_media_L <- vector_tot_L/3 # Luego a la suma de esos vectores, al dividirlos "vector/x", x divide desdel el primer valor hasta el último por separados 

# Cálculo respectivo de 3 mediciones de ETR --> 
vector_1_ <- c(4.2,16.2,29.9,42.3,55.1,71.7,83.4,90.01,88.5)
vect_2_ <- c(1.2,13.1,24.9,30.4,36.7,43.9,48.7,50.9,50.9)
vect_3_ <- c(0.6,12,22.4,29.3,38.3,48.6,56.1,60.9,60.9)

vector_tot_ETR <- vector_1_ +vect_2_ +vect_3_
vector_media_ETR <- vector_tot_ETR/3

# Data frame para la gráfica de ETR/luz

etr <- data.frame(luz = vect_media_L,
                    etr = vector_media_ETR)

etr %>% 
  ggplot(aes(luz, etr)) +
  geom_point(size=2) +
  geom_line(size=1) +
  scale_x_continuous(limits = c(0,1150),
                     breaks = seq(0,1150, 200)) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100, 20)) +
  theme_classic() +
  labs(title = "Estudio del ETR frente a la luz",
       subtitle = "Practicas de Ecofisiología",
       x = "micromoles por otras movidas",
       y = "Luz en Fotones") +
  theme(
    plot.title = element_text(size=16, hjust = .5, face = "bold"),
    plot.subtitle = element_text(size=14, hjust = .5, face = "bold")
  )

########## 2) parte de la practica: sometimiento de las plantas a distintas 
##########    temperaturas para el estudio de su FV/FM parametro de fluorescencia

# vector para la media de los parametros de FV/FM


fvfm1 <- c(.841,.820,.838,.819, NA ,.755,.760,.733)
fvfm2 <- c(.843,.841,.838,.828,.789,.830,.705,.712)
fvfm3 <- c(.843, NA , NA, .827, NA , NA ,.751,0.751)
muestra <- c("muestra1","muestra2","muestra3","muestra4",
             "muestra5","muestra6","muestra7","muestra8")
temp <- c(22,42,44,46,48,50,52,54)
df_fvfm <- tibble(fvfm1=fvfm1,
                  fvfm2=fvfm2,
                  fvfm3=fvfm3,
                  temp=temp,
                  muestra=muestra
                  ) %>% 
  pivot_longer(-muestra) %>%
  filter(name != "temp") %>% 
  group_by(muestra) %>%
  summarise(fvfm_media = mean(value, na.rm=T)) %>%
  mutate(temp=temp)

# Data frame de la segunda parte

df_fvfm %>% 
  ggplot(aes(temp, fvfm_media)) +
  geom_point(size=3) +
  geom_line(size=1) +
  scale_x_continuous(expand = expansion(0),
                     limits = c(20,60),
                     breaks = seq(0,60, 5)) +
  scale_y_continuous(limits = c(0.5,NA)) +
  theme_classic() +
  labs(title = "Medición de la emisión de FV/FM según Tª",
       subtitle = "Práctica de ecofisiología",
       x = "Tª (ºC)",
       y = "FV/FM")

#------------------------------------------------------------------------------#
#                              Practica de 2                                   #
#------------------------------------------------------------------------------#

############# Parte 1

v1 <- c(-.86,.54,1.41,7.21,8.82,7.71)
v2 <- c(-.49,3.74,3.49,6.49,7.43,9.25)
v3 <- c(-1.12,NA,1.43,6.51,11.45,7.08)
par <- c(0,20,50,200,500,800)
muestra <- c("muestra1","muestra2","muestra3",
             "muestra4","muestra5","muestra6")

asim_co2 <- tibble(v1=v1,
                  v2=v2,
                  v3=v3,
                  par=par,
                  muestra=muestra
) %>% 
  pivot_longer(-muestra) %>%
  filter(name != "par") %>% 
  group_by(muestra) %>%
  summarise(asim_co2_media = mean(value, na.rm=T)) %>%
  mutate(par=par)


asim_co2 %>% 
  ggplot(aes(par,asim_co2_media)) +
  geom_point(shape = "triangle", size = 3) +
  geom_line() +
  scale_x_continuous(limits = c(-2,1000),
                     breaks = seq(0,1000, 100)) +
  scale_y_continuous(limits = c(-2,10),
                     breaks = seq(-2,10, 1)) +
  labs(title = "Práctica 2 Viñátigo Regado",
      subtitle = "Práctica de ecofisiología",
       x = "PAR (umol/m^2·s)",
       y = "A (umol CO2/m^2·s)") +
  theme(panel.background = element_blank(),
        axis.line = element_line())



############# Parte 2

data_3_2 <- data.frame(
  regada_noregada = "Regada",
  Potencial_H = -1.166667)
(-1.1-1.2-1.2)/3
data_3_2 %>% 
  ggplot(aes(regada_noregada, Potencial_H)) +
  geom_bar(stat = "identity", width = .2, col = "black", fill = "orange") +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-1.5,0)) +
  scale_y_continuous(expand = expansion(0),
                     breaks = seq(0,-1.5,-.2)) +
  labs(title = "Medición de la emisión de FV/FM según Tª",
       subtitle = "Práctica de ecofisiología",
       y = "Potencial Hídrico",
       x = "Viñátigo Regada") +
  scale_x_discrete(position = "top") +
  theme(
    panel.background = element_blank(),
    axis.ticks.x = element_blank(),
    title = element_text(face = "bold", size = 13),
    axis.line.y = element_line(),
    axis.title = element_text(face = "bold", size = 13),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text("top")
    )


