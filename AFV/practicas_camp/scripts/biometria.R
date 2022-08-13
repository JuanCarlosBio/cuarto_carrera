# Hola aventurero curioso :), si haz logrado descargar R y quieres hacer gráficos chulos sigue estas instrucciones

####################################################################################################
######################## Prácticas de Campo Ecofisiología Biometría del Pinar ######################
####################################################################################################

library(tidyverse) 
library(readxl)
library(ggthemes)

### La primera etapa es usar read_excel y la fuente donde has guardado los datos para importar los datos a R
### Yo lo he utilizado directamente desde mi cuenda de GitHub, hay que hacer una serie de modificaciones
url <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/AFV/practicas_camp/bases_datos/biometria.csv"
biometria <- read_csv(url)

# Vamos a crear los intervalos de DBH que nos permitirá hacer el gráfico

biom_grupos <- biometria %>% 
  mutate(
    intervalos = case_when(dbh > 0 & dbh <= 5 ~ "(0-5]",
                           dbh > 5 & dbh <= 10 ~ "(5-10]",
                           dbh > 10 & dbh <= 15 ~ "(10-15]",
                           dbh > 15 & dbh <= 20 ~ "(15-20]",
                           dbh > 20 & dbh <= 25 ~ "(20-25]",
                           dbh > 25 & dbh <= 30 ~ "(25-30]",       # No se me ha ocurrido algo
                           dbh > 30 & dbh <= 35 ~ "(30-35]",       # más automático :(
                           dbh > 35 & dbh <= 40 ~ "(35-40]",
                           dbh > 40 & dbh <= 45 ~ "(40-45]",
                           dbh > 45 & dbh <= 50 ~ "(45-50]",
                           dbh > 50 & dbh <= 55 ~ "(50-55]",
                           dbh > 55 & dbh <= 60 ~ "(55-60]",
                           dbh > 60 & dbh <= 65 ~ "(60-65]",),
    intervalos = factor(intervalos,
                        levels = c("(0-5]", "(5-10]", "(10-15]", "(15-20]", "(20-25]",
                                   "(25-30]", "(30-35]", "(35-40]", "(40-45]", "(45-50]",
                                   "(50-55]", "(55-60]", "(60-65]"))                      ### Esto ordena los intervalos en orden que queremos en ggplot
  )



biom_grupos %>% 
  group_by(intervalos) %>% 
  summarise(media=mean(altura), sd=sd(altura)) %>%
  ggplot(aes(intervalos, media)) +
  geom_bar(stat = "identity", fill="gray", col="black", width = .5) +
  geom_errorbar(aes(ymin=media+sd, ymax=media-sd), width=.3) +
  labs(title = "DBH frente a la altura, Pino",
       subtitle = "Informe A.F.V,García-Estupiñán, J.C., Biología ULL",
       x = "DBH (cm)",
       y = "Altura (m)") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,25)) +
  theme_tufte() + 
  theme(plot.title=element_text(hjust = .5),
        plot.subtitle=element_text(hjust = .5),
        axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 13, face = "bold"))
  
# En este caso solo se trata de un simple histograma, en el eje x se separan
# los valores de DBH en intervalos de 5, mientras que en el eje y = recuento 
# de los pinos que están en cada intervalo, son pios más maduros que juveniles
# en este caso

hist(biometria$dbh, breaks = 10, xlim = c(0,70),
     main = "Histograma DBH, Pino, Grupo 3", col = "gray",
     xlab = "DBH (cm)", ylab = "Frecuencia", ylim = c(0,10))

