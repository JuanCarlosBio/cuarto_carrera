################################################################################
########### PRÁCTICAS DE INFORMÁTICA DE FISIOLOGÍA ANIMAL APLICADA  ############
################################################################################

library(tidyverse)
library(ggthemes)
library(readxl)
library(rstatix)
library(glue)
library(ggtext)

# # Simulación de los tiempos ratones endogámicos
# generacion0 <- sample(245:300, 12, replace = TRUE)
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

# # Creación de la base de datos
# simulacion <- tibble(
#   engdogamicos = c(generacion0,generacion1,generacion2,generacion3,
#                    generacion4,generacion5,generacion6,generacion7),
#   control = c(controles_0,controles_1,controles_2,controles_3,
#               controles_4,controles_5,controles_6,controles_7)
#   ) %>%
#   mutate(generacion=c(rep("Generación 0", 12),rep("Generación 1", 12),rep("Generación 2", 12),
#                       rep("Generación 3", 12),rep("Generación 4", 12),rep("Generación 5", 12),
#                       rep("Generación 6", 12),rep("Generación 7", 12))) %>%
#   pivot_longer(-generacion, names_to = "tratamiento", values_to = "resultados")
# 
# # Escribir los resulatdos en un csv
# write_csv(simulacion, "simulacion_endogamia.csv") 


#### Resultados que me guarde con write_csv

simulacion <- read_csv("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/trabajo_exposicion/simulacion_endogamia.csv")

# Estudio de la normalidad de los datos:
# generación 0 y 1 no son normales 
simulacion %>% 
  group_by(generacion, tratamiento) %>% 
  shapiro_test(resultados)

# Estudio de la homocedasticidad de los datos
simulacion %>% 
  group_by(generacion) %>% 
  levene_test(resultados ~ as.factor(tratamiento)) %>% 
  filter(p < 0.05)

# Datos normales, homocedasticos
simulacion %>% 
  group_by(generacion) %>% 
  t_test(resultados ~ tratamiento, var.equal = T)

# Datos normales, no homocedásticos
simulacion %>% 
  group_by(generacion) %>% 
  t_test(resultados ~ tratamiento, var.equal = F)

# Datos no normales 
simulacion %>% 
  group_by(generacion) %>% 
  wilcox_test(resultados ~ tratamiento)

simulacion %>% 
  ggplot(aes(resultados, fill=tratamiento)) +
  geom_histogram(alpha=.45, color="black",
                 binwidth = 10)  +
  facet_wrap(~generacion) +
  scale_fill_manual(
    name="Ratones",
    breaks = c("control","engdogamicos"),
    labels = c("Control", "Endogámicos"),
    values = c("black", "orange")) +
  labs(
    title = "Distribución de los datos de cada generación",
    subtitle = "Trabajo de exposición, FAA 2021-2022, ULL",
    x = "Tiempo (s)",
    y = "Frecuencia",
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, size = 14, face = "bold"), 
    plot.subtitle = element_text(hjust = .5, size = 12, face = "italic", 
                                 margin = margin(b=15)), 
    axis.title = element_text(hjust = .5, size = 13, face = "bold"), 
    legend.title = element_text(face = "bold", hjust = .5, size = 11.5),
    legend.text = element_text(size = 11),
    legend.background = element_rect(color="black"),
    legend.position = c(.85, .10)
  )

# ggsave("histogramas.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\trabajo_exposicion\\graficas",
#       width = 7.5, height = 5)

etiquetas <- tibble(
  x = c(1:8),
  y = c(440, 460,470,550,610,610,610,610),
  label = c("", rep("*", 7))
)

simulacion %>% 
  group_by(generacion, tratamiento) %>% 
  summarise(media_tiempo=mean(resultados),
            sd_tiempo=sd(resultados)) %>% 
  ungroup() %>% 
  ggplot(aes(generacion,media_tiempo, group=tratamiento, 
             fill = tratamiento)) +
  geom_line(aes(color=tratamiento), size=1) +
  geom_errorbar(aes(ymin=media_tiempo-sd_tiempo,
                    ymax=media_tiempo+sd_tiempo), width = .3) +
  geom_point(pch=21, size=2) +
  geom_text(data = etiquetas, aes(x=x,y=y,label=label), 
            inherit.aes = FALSE, size=7) +
  scale_y_continuous(limits = c(0,700),
                     breaks = seq(0,700,150)) +
  scale_fill_manual(
    name="Ratones",
    breaks = c("control","engdogamicos"),
    labels = c("Control", "Endogámicos"),
    values = c("black", "orange")) +
  scale_color_manual(
    name="Ratones",
    breaks = c("control","engdogamicos"),
    labels = c("Control", "Endogámicos"),
    values = c("black", "orange")) +
  labs(
    title = "Ratones endogámicos vs control (7 generaciones)",
    subtitle = "Trabajo de exposición, FAA 2021-2022, ULL",
    x = NULL,
    y = "Tiempo (s)"
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1), 
    axis.ticks = element_line(size = 1),
    plot.title = element_text(hjust = .5, size = 14, face = "bold"), 
    plot.subtitle = element_text(hjust = .5, size = 12, face = "italic", 
                                 margin = margin(b=15)), 
    axis.title.y = element_text(hjust = .5, size = 13, face = "bold"), 
    axis.text.x = element_text(angle = 25, hjust = 1, face = "bold",size = 11),
    legend.title = element_text(face = "bold", hjust = .5, size = 11.5),
    legend.text = element_text(size = 11),
    legend.background = element_rect(color="black"),
    legend.position = c(.2, .15)
  )

# ggsave("endogamia.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\trabajo_exposicion\\graficas",
#        width = 7, height = 4.5)

simulación_regression <- simulacion %>% 
  group_by(generacion, tratamiento) %>% 
  summarise(media_tiempo=mean(resultados),
            sd_tiempo=sd(resultados)) %>% 
  ungroup() %>% 
  filter(tratamiento == "engdogamicos") %>% 
  mutate(num_generacion = c(0:7))

# Correlación de Pearson = 0.93
cor.test(simulación_regression$media_tiempo,
         simulación_regression$num_generacion)

# Model de Regresión lineal simple para los ratones
regression_model <- lm(media_tiempo ~ num_generacion, data = simulación_regression)

# p < 0.01*** Modelo es predictivo 
summary(regression_model)

intercepto <- round(regression_model$coefficients[1], 2)
pendiente <- round(regression_model$coefficients[2], 2)


simulación_regression %>% 
  ggplot(aes(num_generacion, media_tiempo)) +
  geom_line(color="black", size=1) +
  geom_errorbar(aes(ymin=media_tiempo-sd_tiempo,
                    ymax=media_tiempo+sd_tiempo), width = .3) +
  geom_point(pch=21, size=2, fill="white") +
  geom_abline(intercept = intercepto, slope = pendiente,
              color="red") +
  geom_text(data = tibble(x = 5, y = 200),
            aes(x=x, y=y, 
                label=glue("Tiempo = Generación·{pendiente} + {intercepto}")),
                inherit.aes=F,size=4.5) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,700),
                     breaks = seq(0,700,150)) +
  scale_x_continuous(
                     breaks = seq(0,7,1)) +
  labs(
    title = "Modelo predictivo ratones endogámicos",
    subtitle = "Trabajo de exposición, FAA 2021-2022, ULL",
    x = "Generación",
    y = "Tiempo (s)"
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1), 
    axis.ticks = element_line(size = 1),
    plot.title = element_text(hjust = .5, size = 14, face = "bold"), 
    plot.subtitle = element_text(hjust = .5, size = 12, face = "italic", 
                                 margin = margin(b=15)), 
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11), 
    legend.title = element_text(face = "bold", hjust = .5, size = 11.5),
    legend.text = element_text(size = 11),
    legend.background = element_rect(color="black"),
    legend.position = c(.2, .15)
  )

ggsave("Rplot09.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\trabajo_exposicion\\graficas",
       width = 7, height = 4.5)

