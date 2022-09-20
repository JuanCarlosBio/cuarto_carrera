################################################################################
########### PRÁCTICAS DE INFORMÁTICA DE FISIOLOGÍA ANIMAL APLICADA  ############
################################################################################

library(tidyverse)
library(ggthemes)
library(readxl)
library(rstatix)
library(glue)
library(ggtext)

# Necesitas información del script de este script. 
source("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/artemias_sripts_datos/inferencia_estadistica.R")

url_artemias <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/artemias_sripts_datos/matrix_artemias.csv"

matrix_artemias <- read_csv(url_artemias)

tidy_artemias <- matrix_artemias %>% 
  mutate(tratamiento = Tratamiento,
         tratamiento = case_when(tratamiento == 1 ~ "Levadura",
                                 tratamiento == 2 ~ "Lectina marina",
                                 tratamiento == 3 ~ "Echium/Bacalao",
                                 tratamiento == 4 ~ "Enriquecedor comercial")) %>% 
  select(-Tratamiento) %>% 
  pivot_longer(-tratamiento, names_to  = "acido_graso", values_to = "valores")

#------------------------------------------------------------------------------#
#                               Visualización de los datos               
#------------------------------------------------------------------------------#

# Resultados del ANOVA de una vía

artemias_anova <- tidy_artemias %>% 
  filter(acido_graso %in% c("C 18:2n-6", "C 18:4n-3", "C 20:4n-6")) %>% 
  mutate(tratamiento=factor(tratamiento, 
                            levels = c("Levadura", "Lectina marina", 
                                       "Echium/Bacalao", "Enriquecedor comercial"),
                            labels = c("Levadura", "Lectina\nmarina", 
                                       "Aceite\nEchium/Bacalao", "Enriquecedor\ncomercial")))



artemias_anova %>% 
  group_by(tratamiento, acido_graso) %>% 
  summarise(media = mean(valores), sd=sd(valores)) %>% 
  ggplot(aes(acido_graso, media, fill=tratamiento)) +
  geom_bar(stat = "identity", position = position_dodge(),
           color="black") +
  geom_errorbar(aes(ymin=media-sd, ymax=media+sd), width=.2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = c("skyblue", "orange", "tomato", "gray")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,10)) +
  labs(
    title = "Cantidad de AG en cada tratamiento",
    subtitle = "Datos normales y homocedásticos",
    x=NULL,
    y="Cantidad AG por Trat.",
    fill="Tratamiento"
  ) +
  theme_classic() +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks.y = element_line(size=1),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = .5,
                                 margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white"),
    axis.text.x = element_text(face = "bold", 
                               color = "black", size = 11)
  )


# ggsave("Rplot03.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\artemias_sripts_datos\\graficas",
#       width = 7, height = 4)

artemias_welch <- tidy_artemias %>% 
  filter(acido_graso %in% c("C 16:1n-7","C 18:0","C 18:1n-9",
                            "C 18:3n-6","C 20:5n-3","C 22:6n-3")) %>% 
  mutate(tratamiento=factor(tratamiento, 
                            levels = c("Levadura", "Lectina marina", 
                                       "Echium/Bacalao", "Enriquecedor comercial"),
                            labels = c("Levadura", "Lectina\nmarina", 
                                       "Aceite\nEchium/Bacalao", "Enriquecedor\ncomercial")))



artemias_welch %>% 
  group_by(tratamiento, acido_graso) %>% 
  summarise(media = mean(valores), sd=sd(valores)) %>% 
  ggplot(aes(acido_graso, media, fill=tratamiento)) +
  geom_bar(stat = "identity", position = position_dodge(),
           color="black") +
  geom_errorbar(aes(ymin=media-sd, ymax=media+sd), width=.2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = c("skyblue", "orange", "tomato", "gray")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,25)) +
  labs(
    title = "Cantidad de AG en cada tratamiento",
    subtitle = "Datos normales y no homocedásticos",
    x=NULL,
    y="Cantidad AG por Trat.",
    fill="Tratamiento"
  ) +
  theme_classic() +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks.y = element_line(size=1),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = .5,
                                 margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white"),
    axis.text.x = element_text(face = "bold", 
                               color = "black", size = 11)
  )

# ggsave("Rplot04.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\artemias_sripts_datos\\graficas",
#       width = 7, height = 4)

artemias_kw <- tidy_artemias %>% 
  filter(acido_graso %in% vect_kw) %>% 
  mutate(tratamiento=factor(tratamiento, 
                            levels = c("Levadura", "Lectina marina", 
                                       "Echium/Bacalao", "Enriquecedor comercial"),
                            labels = c("Levadura", "Lectina\nmarina", 
                                       "Aceite\nEchium/Bacalao", "Enriquecedor\ncomercial")))


artemias_kw %>% 
  ggplot(aes(acido_graso, valores, fill=tratamiento)) +
  # geom_boxplot() +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(seed = 20101997), pch=21) +
  scale_fill_manual(values = c("skyblue", "orange", "tomato", "gray")) +
  facet_wrap(~acido_graso, scales = "free") +
  labs(
    title = "Cantidad de AG en cada tratamiento",
    subtitle = "Datos no normales",
    x=NULL,
    y="Cantidad AG por Trat.",
    fill="Tratamiento"
  ) +
  theme_classic() +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks.y = element_line(size=1),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = .5,
                                 margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white"),
    axis.text.x = element_text(face = "bold", 
                               color = "black", size = 11),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

# ggsave("Rplot06.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\artemias_sripts_datos\\graficas",
#        width = 8, height = 4)
