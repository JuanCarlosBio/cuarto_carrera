################################################################################
########### PRÁCTICAS DE INFORMÁTICA DE FISIOLOGÍA ANIMAL APLICADA  ############
################################################################################

library(tidyverse)
library(ggthemes)
library(readxl)
library(rstatix)
library(glue)
library(ggtext)

matrix_artemias <- read_excel("bases_datos/matrix_artemias.xlsx", 
                              sheet = "Matriz")

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


artemias_welch <- tidy_artemias %>% 
  filter(acido_graso %in% vect_welch) %>% 
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



artemias_kw <- tidy_artemias %>% 
  filter(acido_graso %in% vect_kw) %>% 
  mutate(tratamiento=factor(tratamiento, 
                            levels = c("Levadura", "Lectina marina", 
                                       "Echium/Bacalao", "Enriquecedor comercial"),
                            labels = c("Levadura", "Lectina\nmarina", 
                                       "Aceite\nEchium/Bacalao", "Enriquecedor\ncomercial")))

kw_16_0 <- artemias_kw %>% filter(acido_graso == "C 16:0")
kw_16_3n3 <- artemias_kw %>% filter(acido_graso == "C 18:3n-3")


artemias_kw %>% 
  ggplot(aes(acido_graso, valores, fill=tratamiento)) +
  # geom_boxplot() +
  stat_summary(fun = median, position = position_dodge(.85),
               geom = "crossbar") +
  geom_jitter(position = position_jitterdodge(seed = 20101997), pch=21) +
  scale_fill_manual(values = c("skyblue", "orange", "tomato", "gray")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(8,26)) +
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
  )

