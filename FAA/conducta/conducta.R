################################################################################
###################     Fisiología animal aplicada     #########################
################### Práctica de la conducta de ratones #########################
################################################################################
library(tidyverse)
library(DescTools)
library(readxl)
library(ggthemes)
library(survival) 
library(rstatix)
library(cowplot)
library(glue)

# Importar de un excel con el data frame

url_conducta <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/conducta/conducta_ratones.csv"
conducta <- read_csv(url_conducta)

#------------------------------------------------------------------------------#
#                               OPEN FIELD                                     #
#------------------------------------------------------------------------------#

open_field <- conducta %>% select(starts_with("o_"), "v_j") %>% 
  pivot_longer(-v_j, names_to = "observacion", values_to = "numero_obs") %>% 
  mutate(observacion = case_when(observacion == "o_ambext" ~ "Ambulación externa",
                                 observacion == "o_ambmed" ~ "Ambulación media",
                                 observacion == "o_ambint" ~ "Ambulación interna",
                                 observacion == "o_grooming" ~ "Acicalarse",
                                 observacion == "o_heces" ~ "Heces",
                                 observacion == "o_up" ~ "Up"))

view(of)

# No normales:
# v_j     observacion variable   statistic       p
# Jovenes Acicalarse  numero_obs     0.714 0.00875
# Viejos  Acicalarse  numero_obs     0.640 0.00135
normalidad_of <- open_field %>% 
  group_by(v_j, observacion) %>% 
  shapiro_test(numero_obs) %>% 
  filter(p < 0.05)

no_normales_of <- normalidad_of$observacion

# Todas las variables presenta homocedasticidad
levene_of <- open_field %>% 
  group_by(observacion) %>% 
  mutate(v_j=as.factor(v_j)) %>% 
  levene_test(numero_obs ~ v_j)


# T.test:
# Ningún grupo presenta diferencias significativas
open_field %>% 
  filter(observacion != "Acicalarse") %>% 
  group_by(observacion) %>% 
  t_test(numero_obs ~ v_j, var.equal = T)
 
# Wilcoxon
# Los ratones viejos y jóvenes tampoco presentan 
# fiferencias significativas en Acicalarse
open_field %>% 
  filter(observacion == "Acicalarse") %>% 
  group_by(observacion) %>% 
  wilcox_test(numero_obs ~ v_j)

# gráficos de las ambulaciones

of1 <- open_field %>%
  filter(observacion %in% c("Ambulación interna", "Ambulación media", "Ambulación externa")) %>% 
  mutate(ambulacion = factor(observacion,
                             levels = c("Ambulación interna", "Ambulación media", 
                                        "Ambulación externa"),
                             labels = c("Ambulación\ninterna", "Ambulación\nmedia", 
                                        "Ambulación\nexterna"))) %>%
  select(-observacion) %>% 
  group_by(v_j, ambulacion) %>% 
  summarise(media=mean(numero_obs), sd=sd(numero_obs), .groups = "drop") %>% 
  ggplot(aes(v_j, media, fill=reorder(ambulacion, media))) +
  geom_bar(stat = "identity", color="black",
           width = .5, position = position_dodge(.7)) +
  geom_errorbar(aes(ymin=media-0, ymax=media+sd), 
                width=.3, position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,160),
                     breaks = seq(0,160, 30)) +
  scale_fill_manual(values = c("white", "black", "orange")) +
  labs(title = "Ambulación Open field",
       subtitle = "Datos Normales y homocedásticos: T.test: p > 0.05 todos",
       #caption = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Ambulación",
       x = NULL,
       fill = "Ambulación") +
  theme_classic() +
  theme(axis.line = element_line(size = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 9, hjust = .5),
        plot.title = element_text(face = "bold", size = 13, hjust = .5),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = .5),
        plot.caption = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 13, color="black"),
        axis.ticks.x = element_blank() ,
        axis.ticks.y = element_line(size=1)
        )

of2 <- open_field %>%
  filter(observacion %in% c("Heces", "Up")) %>% 
  group_by(v_j, observacion) %>% 
  summarise(media=mean(numero_obs), sd=sd(numero_obs), .groups = "drop") %>% 
  ggplot(aes(v_j, media, fill=reorder(observacion, media))) +
  geom_bar(stat = "identity", color="black",
           width = .5, position = position_dodge(.7)) +
  geom_errorbar(aes(ymin=media-0, ymax=media+sd), 
                width=.3, position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,NA)) +
  scale_fill_manual(values = c("brown", "skyblue")) +
  labs(title = "Comportamientos específicos Open field",
       subtitle = "Datos Normales y homocedásticos: T.test: p > 0.05 todos",
       #caption = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Frecuencia",
       x = NULL,
       fill = "Comportamiento") +
  theme_classic() +
  theme(axis.line = element_line(size = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 9, hjust = .5),
        plot.title = element_text(face = "bold", size = 11, hjust = .5),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = .5),
        plot.caption = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 13, color="black"),
        axis.ticks.x = element_blank() ,
        axis.ticks.y = element_line(size=1)
  )
  
of3 <- open_field %>%
  filter(observacion %in% c("Acicalarse")) %>% 
  ggplot(aes(v_j, numero_obs, fill=reorder(observacion, numero_obs))) +
  geom_boxplot(width=.5, alpha=.5) +
  geom_jitter(pch=21, position = position_jitterdodge(seed = )) +
  scale_y_continuous(limits = c(-1,NA)) +
  scale_fill_manual(values = c("yellow", "skyblue")) +
  labs(title = "Comportamientos específicos Open field",
       subtitle = "Datos NO Normales y homocedásticos: T.test: p > 0.05 todos",
       caption = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Frecuencia",
       x = NULL,
       fill = "Comportamiento") +
  theme_classic() +
  theme(axis.line = element_line(size = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 9, hjust = .5),
        plot.title = element_text(face = "bold", size = 11, hjust = .5),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = .5),
        plot.caption = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 13, color="black"),
        axis.ticks.x = element_blank() ,
        axis.ticks.y = element_line(size=1)
  )

plot_grid(of1,of2,of3,
                   labels = c("A","B","C"))

# ggsave("om.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\conducta\\graficas",
#       width = 8.5, height = 6)

#------------------------------------------------------------------------------#
#                                  Holes maze                                  #
#------------------------------------------------------------------------------#
holes_maze <- conducta %>% select(starts_with("h_"), "v_j") %>% 
  pivot_longer(-v_j, names_to = "observacion", values_to = "numero_obs") %>% 
  mutate(observacion = case_when(observacion == "h_amb" ~ "Ambulación",
                                 observacion == "h_cabeza" ~ "Mete cabeza",
                                 observacion == "h_t_oler" ~ "Tiempo olisqueo",
                                 observacion == "h_grooming" ~ "Acicalarse",
                                 observacion == "h_up" ~ "Up"))

# Todos son normales:
normalidad_hm <- holes_maze %>% 
  filter(observacion != "Acicalarse") %>%    # Todos son cero, no existen diff pero... tiene que ser error xd
  group_by(v_j, observacion) %>% 
  shapiro_test(numero_obs) #%>% 
  # filter(p < 0.05)

# Todas las variables presenta homocedasticidad
levene_of <- holes_maze %>% 
  filter(observacion != "Acicalarse") %>%    
  group_by(observacion) %>% 
  mutate(v_j=as.factor(v_j)) %>% 
  levene_test(numero_obs ~ v_j)


# T.test:
# observacion   group1        p
# Mete cabeza  Jovenes     0.00297
holes_maze %>% 
  filter(observacion != "Acicalarse") %>% 
  group_by(observacion) %>% 
  t_test(numero_obs ~ v_j, var.equal = T) %>% 
  filter(p < .05) %>% select(observacion, group1, group1, p  )


# gráfico Hole Maze

etiqueta_sighm <- tibble(x=c(1,2),
                       y=c(42,60),
                       color=c(F,T),
                       etiqueta=c("***","***"))

holes_maze %>%
  filter(observacion != "Acicalarse") %>% 
  mutate(ambulacion = factor(observacion,
                             levels = c("Ambulación", "Mete cabeza", 
                                        "Tiempo olisqueo", "Up"),
                             labels = c("Ambulación", "Mete\ncabeza", 
                                        "Tiempo\nolisqueo", "Up"))) %>%
  group_by(v_j, observacion) %>% 
  summarise(media=mean(numero_obs), sd=sd(numero_obs), .groups = "drop") %>%
  mutate(mucho=observacion=="Tiempo olisqueo") %>% 
  ggplot(aes(v_j, media, fill=reorder(observacion, media))) +
  geom_bar(stat = "identity", color="black",
           width = .5, position = position_dodge(.7)) +
  geom_errorbar(aes(ymin=media-0, ymax=media+sd), 
                width=.3, position = position_dodge(.7)) +
  geom_hline(yintercept = 0, size=1) +
  geom_text(data = etiqueta_sighm,
            aes(x=x, y=y, label=etiqueta),
            inherit.aes = FALSE, size=5) +
  facet_wrap(~mucho, ncol = 1, scales = "free") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,NA)) +
  scale_fill_manual(breaks = c("Ambulación", "Mete cabeza", 
                               "Tiempo olisqueo", "Up"),
                    labels = c("Ambulación", "Mete\ncabeza", 
                               "Tiempo\nolisqueo", "Up"),
                    values = c("white", "darkgray", "black","orange", "red")) +
  labs(title = "Observaciones Holemaze Open field",
       subtitle = "Datos Normales y homocedásticos: T.test: Meter la cabeza***",
       caption = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Observación",
       x = NULL,
       fill = NULL) +
  theme_classic() +
  theme(axis.line = element_line(size = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 10, hjust = .5),
        plot.title = element_text(face = "bold", size = 13, hjust = .5),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = .5),
        plot.caption = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13, color="black"),
        axis.ticks.x = element_blank() ,
        axis.ticks.y = element_line(size=1),
        strip.background = element_blank(),
        strip.text = element_blank()
  )

# ggsave("hm.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\conducta\\graficas",
#       width = 7, height = 5.5)
#------------------------------------------------------------------------------#
#                                 P.Maze                                       #
#------------------------------------------------------------------------------#

p.maze <- conducta %>% select(starts_with("p_"), "v_j") %>% 
  pivot_longer(-v_j, names_to = "observacion", values_to = "numero_obs") %>%
  mutate(observacion = case_when(observacion == "p_t_cto" ~ "Tiempo centro",
                                 observacion == "p_t_ba" ~ "Tiempo abierto",
                                 observacion == "p_t_bc" ~ "Tiempo cerrado",
                                 observacion == "p_cto" ~ "Nº.centro",
                                 observacion == "p_ba" ~ "Nº.abierto",
                                 observacion == "p_bc" ~ "Nº.cerrado",
                                 observacion == "p_grooming" ~ "Acicalarse", 
                                 observacion == "p_up" ~ "Up",
                                 )) %>% 
  filter(numero_obs < 500) # Hay uno serie de otliers en el tiempo que pasan en cada brazo 

# No normales:
normalidad_pm <- p.maze %>% 
  filter(observacion != "Acicalarse") %>%    # Los viejos no se acicalan BUG del juego
  group_by(v_j, observacion) %>% 
  shapiro_test(numero_obs) %>% 
  filter(p < 0.05)

no_normales_pm <- unique(normalidad_pm$observacion)

# Todas las variables presenta homocedasticidad
levene_pm <- p.maze %>% 
  group_by(observacion) %>% 
  mutate(v_j=as.factor(v_j)) %>% 
  levene_test(numero_obs ~ v_j) %>% 
  filter(p < 0.05)

no_homoc_pm <- unique(levene_pm$observacion)

# T.test: No ha diferencias
# observacion   p
# Acicalarse   0.0779
# Nº.cerrado   0.137 
# Up           0.917 

p.maze %>% 
  filter(!(observacion %in% no_normales_pm) & 
           !(observacion %in% no_homoc_pm)) %>% 
  group_by(observacion) %>% 
  t_test(numero_obs ~ v_j, var.equal = T)

# T.Welch:
# observacion      p
#  Nº.centro   0.0294 **
p.maze %>% 
  filter(observacion == no_homoc_pm) %>% 
  group_by(observacion) %>% 
  t_test(numero_obs ~ v_j, var.equal = F) %>% select(observacion, p)


# Wilcoxon
# Los ratones viejos y jóvenes tampoco presentan 
# fiferencias significativas en Acicalarse
p.maze %>% 
  filter(observacion %in% no_normales_pm) %>% 
  group_by(observacion) %>% 
  wilcox_test(numero_obs ~ v_j)

# gráficos de los tiempos

p1 <- p.maze %>%
  filter(observacion %in% c("Tiempo centro", "Tiempo cerrado")) %>% 
  mutate(observacion = factor(observacion,
                             levels = c("Tiempo centro", 
                                        "Tiempo cerrado"),
                             labels = c("Centro", 
                                        "Cerrado"))) %>%
  group_by(observacion, v_j) %>%
  summarise(media=mean(numero_obs), sd=sd(numero_obs)) %>%
  ggplot(aes(v_j, media, fill=observacion)) +
  geom_bar(stat = "identity", color="black",
           width = .5, position = position_dodge(.7)) +
  geom_errorbar(aes(ymin=media-0, ymax=media+sd), 
                width=.3, position = position_dodge(.7)) +
  # geom_boxplot(data=data_abierto,aes(v_j, numero_obs), inherit.aes = FALSE,
  #              fill="orange", alpha=.25, width=.25, position = position_nudge(x=.5,y=0)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,200),
                     breaks = seq(0,200, 40)) +
  scale_fill_manual(values = c("white", "black", "orange")) +
  labs(title = "Tiempo de estancia (centro y cerrado) P.maze",
       subtitle = "Datos Normales y Homocedásticos: T.test: p > 0.05 todos",
       #caption = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Tiempo (seg)",
       x = NULL,
       fill = "Lugar de\nestancia") +
  theme_classic() +
  theme(axis.line = element_line(size = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 9, hjust = .5),
        plot.title = element_text(face = "bold", size = 13, hjust = .5),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = .5),
        plot.caption = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 13, color="black"),
        axis.ticks.x = element_blank() ,
        axis.ticks.y = element_line(size=1)
  );p1

p2 <- p.maze %>%
  filter(observacion %in% c("Tiempo abierto")) %>% 
  mutate(observacion = factor(observacion,
                              levels = c("Tiempo abierto"),
                              labels = c("Aabierto"))) %>%
  ggplot(aes(v_j, numero_obs, fill=observacion)) +
  geom_boxplot(width=.35, alpha=.25) +
  geom_jitter(pch=21,position = position_jitterdodge(.5), size=1) +
  scale_y_continuous(limits = c(0,200),
                     breaks = seq(0,200, 40)) +
  scale_fill_manual(values ="orange") +
  labs(title = "Tiempo de estancia (abierto) P.maze",
       subtitle = "Datos NO Normales: Wicoxon: p > 0.05",
       #caption = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Tiempo (seg)",
       x = NULL,
       fill = "Lugar de\nestancia") +
  theme_classic() +
  theme(axis.line = element_line(size = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 9, hjust = .5),
        plot.title = element_text(face = "bold", size = 13, hjust = .5),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = .5),
        plot.caption = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 13, color="black"),
        axis.ticks.x = element_blank() ,
        axis.ticks.y = element_line(size=1)
  );p2

pmix1 <- plot_grid(p1,p2,nrow = 1,
          labels = c("A","B"))

### Gráficos de estancias:

p3 <- p.maze %>%
  filter(observacion %in% c("Nº.centro", "Nº.cerrado")) %>% 
  mutate(observacion = factor(observacion,
                              levels = c("Nº.centro", 
                                         "Nº.cerrado"),
                              labels = c("Centro","Brazo\ncerrado"))) %>%
  group_by(observacion, v_j) %>%
  summarise(media=mean(numero_obs), sd=sd(numero_obs)) %>%
  ggplot(aes(v_j, media, fill=observacion)) +
  geom_bar(stat = "identity", color="black",
           width = .5, position = position_dodge(.7)) +
  geom_errorbar(aes(ymin=media-0, ymax=media+sd), 
                width=.3, position = position_dodge(.7)) +
  geom_text(data = tibble(x=c(.825,1.825),y=c(12,6)),
            aes(x=x,y=y, label="*"), inherit.aes = FALSE, size=7) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,20),
                     breaks = seq(0,20, 3)) +
  scale_fill_manual(values = c("tomato", "skyblue")) +
  labs(title = "Estancia (centro y cerrado) P.maze",
       subtitle = "Datos Normales, Homocedásticos (centro) y\nno Homocedásticos (cerrado)",
       #caption = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Número de estancias",
       x = NULL,
       fill = "Lugar de\nestancia") +
  theme_classic() +
  theme(axis.line = element_line(size = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 9, hjust = .5),
        plot.title = element_text(face = "bold", size = 13, hjust = .5),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = .5),
        plot.caption = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 13, color="black"),
        axis.ticks.x = element_blank() ,
        axis.ticks.y = element_line(size=1)
  );p3

p4 <- p.maze %>%
  filter(observacion %in% c("Nº.abierto")) %>% 
  mutate(observacion = factor(observacion,
                              levels = c("Nº.abierto"),
                              labels = c("Brazo\nabierto"))) %>%
  ggplot(aes(v_j, numero_obs, fill=observacion)) +
  geom_boxplot(width=.35, alpha=.25) +
  geom_jitter(pch=21,position = position_jitterdodge(.5), size=1) +
  scale_y_continuous(limits = c(0,20),
                     breaks = seq(0,20, 3)) +
  scale_fill_manual(values ="yellowgreen") +
  labs(title = "Estancia (abierto) P.maze",
       subtitle = "Datos NO Normales: Wicoxon: p > 0.05",
       #caption = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Número de estancias",
       x = NULL,
       fill = "Lugar de\nestancia") +
  theme_classic() +
  theme(axis.line = element_line(size = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 9, hjust = .5),
        plot.title = element_text(face = "bold", size = 13, hjust = .5),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = .5),
        plot.caption = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 13, color="black"),
        axis.ticks.x = element_blank() ,
        axis.ticks.y = element_line(size=1)
  );p4

pmix2 <- plot_grid(p3,p4,nrow = 1,
          labels = c("C","D"))

plot_grid(pmix1,pmix2, nrow = 2)

# ggsave("Rplot05.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\conducta\\graficas",
#       width = 8.75, height = 6)


###### Observaciones específicos del p.maiz

p.maze %>%
  filter(observacion %in% c("Acicalarse", "Up")) %>% 
  mutate(observacion = factor(observacion,
                              levels = c("Acicalarse", 
                                         "Up"))) %>%
  group_by(observacion, v_j) %>%
  summarise(media=mean(numero_obs), sd=sd(numero_obs)) %>%
  ggplot(aes(v_j, media, fill=observacion)) +
  geom_bar(stat = "identity", color="black",
           width = .5, position = position_dodge(.7)) +
  geom_errorbar(aes(ymin=media-0, ymax=media+sd), 
                width=.3, position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,30),
                     breaks = seq(0,30, 3)) +
  scale_fill_manual(values = c("tomato", "skyblue")) +
  labs(title = "Observaciones específicas P.maze",
       subtitle = "Datos Normales y Homocedásticos; p > 0.05 Ups. Obviamente hay diferencias en cuanto a\nlos acicalamientos, pero al haber 0 de los viejos, no se pudierno hacer de forma correcata los test.",
       #caption = "Práctica de conducta FAA, G.2 105b-106, 4º Biología ULL",
       y = "Frecuencia",
       x = NULL,
       fill = "comportamiento\nespecífico") +
  theme_classic() +
  theme(axis.line = element_line(size = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 9, hjust = .5),
        plot.title = element_text(face = "bold", size = 13, hjust = .5),
        plot.subtitle = element_text(face = "italic", size = 8.5, hjust = .5),
        plot.caption = element_text(face = "italic", size = 11),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 13, color="black"),
        axis.ticks.x = element_blank() ,
        axis.ticks.y = element_line(size=1)
  )

#---------------------------------------------------------------#
# Análisis de componentes principales para cada prueba          #
#---------------------------------------------------------------#

# matrices:
matrix_of <- conducta %>% select(starts_with("o_"), "v_j")
matrix_hm <- conducta %>% select(starts_with("h_"), "v_j")
matrix_pm <- conducta %>% select(starts_with("p_"), "v_j")

##### PCA Open Field
matrix_numof <- matrix_of[,-7]

pca_of <- prcomp(matrix_numof, scale=TRUE, center=TRUE)

resumen_of <- summary(pca_of) 
cor(pca_of$x[,c(1,2)], matrix_numof)

# Componentes principales Open Field
pc_of <- as_tibble(pca_of$x[,c(1,2)]) %>% 
  mutate(v_j=matrix_of$v_j)
# Varianza explicada por PC1 y PC2

pc1_of <- round(resumen_of$importance[2,1]*100,2)
pc2_of <- round(resumen_of$importance[2,2]*100,2)

pc_1 <- pc_of %>% 
  ggplot(aes(PC1, PC2, fill=v_j, color = v_j)) +
  geom_point(pch=21, color="black") +
  stat_ellipse(geom = "polygon", alpha = .25) +
  geom_vline(xintercept = 0, color="black", linetype="dashed") +
  geom_hline(yintercept = 0, color="black", linetype="dashed") +
  scale_fill_manual(name="Edad:",
                    values = c("red", "blue")) + 
  scale_color_manual(name="Edad:",
                     values = c("red", "blue")) +
  labs(
    title = "PCA para Las Variables de Open Field",
    x=glue("PC1 ({pc1_of}% varianza explicada)"),
    y=glue("PC2 ({pc2_of}% varianza explicada)")
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks = element_line(size=1),
    plot.title = element_text(size = 14, face = "bold", hjust = .5,
                              margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white")
  )

##### PCA Hole maez
matrix_numhm <- matrix_hm[,-6]

pca_hm <- prcomp(matrix_numhm, scale=TRUE, center=TRUE)

resumen_hm <- summary(pca_hm) 
cor(pca_hm$x[,c(1,2)], matrix_numhm)

# Componentes principales Open Field
pc_hm <- as_tibble(pca_hm$x[,c(1,2)]) %>% 
  mutate(v_j=matrix_hm$v_j)
# Varianza explicada por PC1 y PC2

pc1_hm <- round(resumen_hm$importance[2,1]*100,2)
pc2_hm <- round(resumen_hm$importance[2,2]*100,2)

pc_2 <- pc_hm %>% 
  ggplot(aes(PC1, PC2, fill=v_j, color = v_j)) +
  geom_point(pch=21, color="black") +
  stat_ellipse(geom = "polygon", alpha = .25) +
  geom_vline(xintercept = 0, color="black", linetype="dashed") +
  geom_hline(yintercept = 0, color="black", linetype="dashed") +
  scale_fill_manual(name="Edad:",
                    values = c("red", "blue")) + 
  scale_color_manual(name="Edad:",
                     values = c("red", "blue")) +
  labs(
    title = "PCA para Las Variables de Hole Maze",
    x=glue("PC1 ({pc1_hm}% varianza explicada)"),
    y=glue("PC2 ({pc2_hm}% varianza explicada)")
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks = element_line(size=1),
    plot.title = element_text(size = 14, face = "bold", hjust = .5,
                              margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white")
  )

# Componentes principales Open Field
pc_of <- as_tibble(pca_of$x[,c(1,2)]) %>% 
  mutate(v_j=matrix_of$v_j)
# Varianza explicada por PC1 y PC2

pc1_of <- round(resumen_of$importance[2,1]*100,2)
pc2_of <- round(resumen_of$importance[2,2]*100,2)

pc_of %>% 
  ggplot(aes(PC1, PC2, fill=v_j, color = v_j)) +
  geom_point(pch=21, color="black") +
  stat_ellipse(geom = "polygon", alpha = .25) +
  geom_vline(xintercept = 0, color="black", linetype="dashed") +
  geom_hline(yintercept = 0, color="black", linetype="dashed") +
  scale_fill_manual(name="Edad:",
                    values = c("red", "blue")) + 
  scale_color_manual(name="Edad:",
                     values = c("red", "blue")) +
  labs(
    title = "PCA para Las Variables de Open Field",
    x=glue("PC1 ({pc1_of}% varianza explicada)"),
    y=glue("PC2 ({pc2_of}% varianza explicada)")
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks = element_line(size=1),
    plot.title = element_text(size = 14, face = "bold", hjust = .5,
                              margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white")
  )

##### PCA Hole maez
matrix_numpm <- matrix_pm[,-9]

pca_pm <- prcomp(matrix_numpm, scale=TRUE, center=TRUE)

resumen_pm <- summary(pca_pm) 
cor(pca_pm$x[,c(1,2)], matrix_numpm)

# Componentes principales P.maze
pc_pm <- as_tibble(pca_pm$x[,c(1,2,3)]) %>% 
  mutate(v_j=matrix_pm$v_j)
# Varianza explicada por PC1 y PC2

pc1_pm <- round(resumen_pm$importance[2,1]*100,2)
pc2_pm <- round(resumen_pm$importance[2,2]*100,2)
pc3_pm <- round(resumen_pm$importance[2,3]*100,2)


pc_3 <- pc_pm %>% 
  ggplot(aes(PC1, PC2, fill=v_j, color = v_j)) +
  geom_point(pch=21, color="black") +
  stat_ellipse(geom = "polygon", alpha = .25) +
  geom_vline(xintercept = 0, color="black", linetype="dashed") +
  geom_hline(yintercept = 0, color="black", linetype="dashed") +
  scale_fill_manual(name="Edad:",
                    values = c("red", "blue")) + 
  scale_color_manual(name="Edad:",
                     values = c("red", "blue")) +
  labs(
    title = "PCA para Las Variables de P.maze",
    x=glue("PC1 ({pc1_pm}% varianza explicada)"),
    y=glue("PC2 ({pc2_pm}% varianza explicada)")
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks = element_line(size=1),
    plot.title = element_text(size = 14, face = "bold", hjust = .5,
                              margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white")
  )


library(plotly)

plot_ly(pc_pm, 
        x = ~PC1, y = ~PC2, z = ~PC3, 
        color = ~v_j, 
        colors = c('red','blue')
)%>%
  add_markers(size = 15) %>%
  layout(
    title = glue('Total Explained Variance = {pc1_pm+pc2_pm+pc3_pm}%'),
    scene = list(bgcolor = "white")
  )

plot_grid(pc_1, pc_2, pc_3)

ggsave("Rplot07.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\conducta\\graficas",
       width = 8, height = 7)
