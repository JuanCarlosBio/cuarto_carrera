#------------------------------------------------------#
# Prácticas de Campo Ecofisiología Biometría del Pinar #
#------------------------------------------------------#

library(tidyverse) 

# Leemos nuestros datos
biometria <- read_csv("data/biometria.csv")

# Vamos a crear los intervalos de DBH que nos permitirá hacer el gráfico

biom_grupos <- biometria %>% 
  mutate(
    intervalos = case_when(dbh > 0 & dbh <= 5 ~ "(0-5]",
                           dbh > 5 & dbh <= 10 ~ "(5-10]",
                           dbh > 10 & dbh <= 15 ~ "(10-15]",
                           dbh > 15 & dbh <= 20 ~ "(15-20]",
                           dbh > 20 & dbh <= 25 ~ "(20-25]",
                           dbh > 25 & dbh <= 30 ~ "(25-30]",
                           dbh > 30 & dbh <= 35 ~ "(30-35]",
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
  theme_minimal() + 
  theme(plot.title=element_text(hjust = .5),
        plot.subtitle=element_text(hjust = .5),
        axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 13, face = "bold"))

  ggsave("results/plots/biometria/01dbh_altura.png",
         width = 7,
         height = 5)
  
# En este caso solo se trata de un simple histograma, en el eje x se separan
# los valores de DBH en intervalos de 5, mientras que en el eje y = recuento 
# de los pinos que están en cada intervalo, son pios más maduros que juveniles
# en este caso
png("results/plots/biometria/02hist_dbh.png",
    width = 1500,
    height = 1200,
    res = 300)

hist(biometria$dbh, breaks = 10, xlim = c(0,70),
     main = "Histograma DBH, Pino, Grupo 3", col = "gray",
     xlab = "DBH (cm)", ylab = "Frecuencia", ylim = c(0,10))

