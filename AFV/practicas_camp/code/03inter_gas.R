################################################################################
#########################    Intercambio gaseoso             ###################
################################################################################
library(tidyverse)
library(glue)
library(DescTools)

# Este link provee un script donde tengo una funciones interesantes hechas por mí
source("https://raw.githubusercontent.com/Juankkar/cosas_mias/main/funciones_propias/funR/contraste_hip.R")
#------------------------------------------------------------------------------#
#                       Estudio de la Tª con/sin vaselina                      #
#------------------------------------------------------------------------------#

fvfm <- read_csv("data/fvfm.csv") %>% 
  mutate(hora=as.character(hora))
 
temp_sol <- fvfm %>% filter(exposicion %in% "Sol");temp_sol
temp_sombra <- fvfm %>% filter(exposicion %in% "Sombra");temp_sombra

temp_sol %>% 
  group_by(hora,vaselina) %>% 
  mutate(media = mean(temperatura), sd = sd(temperatura)) %>% 
  ggplot(aes(hora, media, fill = vaselina)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.9)) +
  geom_text(data = tibble(x=c(1,3,5), y=c(20,19,37)),
            aes(x=x,y=y,label=c(rep("*",3))), inherit.aes = F, size=8) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(title = "Tª de la hoja con/sin vaselina, Rosalillo al sol",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "T leaf (ºC)",
       fill = "Con/sin vaselina") +
  theme_classic() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.3,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))

ggsave("results/plots/intercambio_gaseoso/09T_vaselina_hojas_sol.png",
       width = 7,
       height = 5)

temp_sombra %>% 
  group_by(hora,vaselina) %>% 
  summarise(media = mean(temperatura), sd = sd(temperatura)) %>% 
  ggplot(aes(hora, media, fill = vaselina)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.9)) +
  geom_text(data=tibble(x=6, y=12),
            aes(x=x,y=y, label="*"), size=10, inherit.aes=FALSE) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(title = "Tª de la hoja con/sin vaselina, Rosalillo a la sombra",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "T leaf (ºC)",
       fill = "Con/sin vaselina") + 
  theme_classic() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.7,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))

ggsave("results/plots/intercambio_gaseoso/10T_vaselina_hojas_som.png",
       width = 7,
       height = 5)

#####################################
# Análisis estadístico más profundo # Se debería buscar alguna forma de hacer este proceso más auto_
##################################### mático, tiempo al tiempo supongo.

## Función para el análisis inferencial
inferencia_bucle <- function(data){
  for(i in horas){
    print(glue(">>> Hora de estudio para la comparación: {i}"))
    vector = data %>% filter(hora %in% i)
    print(tw.groups(vector, temperatura, "temperatura", vaselina,"Con vaselina","Sin vaselina"))
  }
}

## Horas de estudio
horas <- c("10:00:00", "10:30:00","11:00:00",
           "11:30:00", "12:00:00", "12:30:00")

## Temperatura al sol
temp_sol2 <- temp_sol[,c("hora","temperatura","vaselina")]
## Temperatura a la sombra
temp_sombra2 <- temp_sombra[,c("hora","temperatura","vaselina")]

## Resultados Temperatura luz
inferencia_bucle(temp_sol2)
## Resultados Temperatura sombra
inferencia_bucle(temp_sombra2)

#------------------------------------------------------------------------------#
#                         Eficiencia fotoquímica del                           #
#                             Pino y el rosalillo                              #
#------------------------------------------------------------------------------#

fvfm2 <- read_csv("data/fvfm2.csv") %>% 
  mutate(hora=as.character(hora))

fvfm2 %>%
  group_by(hora, exposicion) %>% 
  summarise(media = mean(fvfm), sd = sd(fvfm)) %>% 
  ggplot(aes(hora, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.6), 
           col = "black", width = .5) +
  geom_errorbar(aes(ymin = media-sd, ymax = media + sd), width = .2,
                position = position_dodge(.6)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1.3),
                     breaks = seq(0,1.3, .2)) +
  labs(title = "Eficiencia fotoquímica Pino/Rosalillo",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "Fv/Fm",
       fill = "Sol/Sombra") +
  theme_classic() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.2,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_color_manual(values = c("white", "gray"))

ggsave("results/plots/intercambio_gaseoso/11ef_foto_rosalillo.png",
       width = 7,
       height = 5)

#------------------------------------------------------#
# Son las diferencias estadísticamente significativas? #
#------------------------------------------------------#

for(i in c("Pino", "Rosalillo")){
  print(glue(">>> Inferencia de la especie: {i}"))
  df <- fvfm2 %>% filter(especie %in% i)
  tapply(df$fvfm, df$exposicion, shapiro.test)
  LeveneTest(fvfm~as.factor(exposicion), data = df)          
  print(t.test(fvfm~exposicion, data = df)) 
}

